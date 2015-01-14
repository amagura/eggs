// (Readline is GPLed, so that makes this file GPLed too, because this
// file will only run if it's linked against readline.)
//
// Copyright (c) 2002 Tony Garnock-Jones
// Copyright (c) 2006 Heath Johns (paren bouncing and auto-completion code)
// Copyright (c) 2015 Alexej Magura (added better prompt selection, ...)
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/poll.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <stdbool.h>

/** Macros - BEGIN **/
/*** Conditional Macros ***/
#ifndef INLINE
#ifdef __inline__
#define INLINE __inline__
#elif defined __GNUC__ && defined __GNUC_GNU_INLINE__
#define INLINE __GNUC_GNU_INLINE__
#else
#define INLINE inline
#endif
#endif

#ifndef DEBUG
#define DEBUG 0
#endif

#if DEBUG /* NOTE change this to 1 to enable debug messages */
#define RL_EGG_TRACE(format, ...) \
  do { \
    fprintf(stderr, "++ %s(%d):\n", __FILE__, __LINE__); \
    fprintf(stderr, (format), (__VA_ARGS__)); \
    fprintf(stderr, "\n-- %s %s `%s'\n\n", "In", "function", __FUNCTION__); \
  } while(0)
#define RL_EGG_DEBUG(format, ...) RL_EGG_TRACE((format), (__VA_ARGS__))
#else
#define RL_EGG_TRACE(format, ...)
#define RL_EGG_DEBUG(format, ...)
#endif

#if _HAVE_LIBBSD /* then we can use strlcat */
#include <bsd/string.h>
#define RL_EGG_STRCAT(DESTINATION_STR, SOURCE_STR) \
  strlcat((DESTINATION_STR), (SOURCE_STR), sizeof (DESTINATION_STR))
#else /* else we have to use strncat */
#define RL_EGG_STRCAT(DESTINATION_STR, SOURCE_STR) \
  strncat((DESTINATION_STR), (SOURCE_STR), (sizeof (DESTINATION_STR) - strlen((DESTINATION_STR)) - 1))
#endif
/** Macros - END **/

struct delim_count {
  int open;
  int close;
};

static int gnu_readline_bounce_ms = 500; // FIXME I'm a global variable... I think.
static int gnu_history_newlines = 0; // FIXME I'm a global
static char *gnu_readline_buf = NULL; // FIXME I'm a global

#if 0 /* NOT YET IMPLEMENTED */
static struct gnu_readline_current_paren_color_t {
  int enabled;
  char *with_match;
  char *without_match;
} paren_colors;
#endif

/* XXX readline already provides paren-bouncing:
   (gnu-readline-parse-and-bind "set blink-matching-paren on")

   XXX it however does ____NOT____ work.  built-in: (line 'a) ')
                                                    ^ -------- ^ ; WRONG
   ~ Alexej
*/
INLINE char
peek_chr(char *cp, char *stringp, bool direct)
{
  if (direct) {
    char next = '\0';
    if (cp != &stringp[strlen(stringp) - 1]) {
      next = *(++cp);
      --cp;
    } else {
      next = *cp;
    }
    return next;
  } else {
    char prev = '\0';
    if (cp != stringp) {
      prev = *(--cp);
      ++cp;
    } else {
      prev = *cp;
    }
    return prev;
  }
}

INLINE int *
strnof_delim(char *str, const char open_delim, const char close_delim, int *idx)
{
  char *cp = &str[strlen(str)];

  if (idx == NULL) {
    int kdx[2];
    idx = kdx;
  }
  if (cp == str)
    return NULL;

  do {
    if (cp != str && peek_chr(cp, str, false) == '\\')
      continue;

    if (*cp == close_delim)
      ++idx[1];
    else if (*cp == open_delim)
      ++idx[0];
  } while(cp-- != str);

  RL_EGG_DEBUG("open: %d\nclose:%d\n", idx[0], idx[1]);

  if (open_delim == close_delim && idx[1] > 0) {
    if (idx[0] % 2 == 1)
      while(idx[0]++ < --idx[1]);
    else
      idx[1]%= 2;
  }
  RL_EGG_DEBUG("open: %d\nclose:%d\n", idx[0], idx[1]);

  return idx;
}

#if 0 // NOT YET IMPLEMENTED
int
highlight_paren()
{
  char *rl_ptr = rl_line_buffer;
  while (rl_ptr != &rl_line_buffer[rl_point]) { ++rl_ptr; }
  _CHKSCM_RL_EGG_DEBUG("%s\n", rl_ptr);
  return 0;
}
#endif

////\\\\//// Paren Bouncing ////\\\\////

/* Returns: (if positive) the position of the matching paren
            (if negative) the number of unmatched closing parens */

// Finds the matching paren (starting from just left of the cursor)
INLINE int
gnu_readline_skip(int pos, const char open_key, const char close_key)
{
  while (--pos > -1) {
    if (pos > 0 && rl_line_buffer[pos - 1] == '\\')
      continue;
    else if (rl_line_buffer[pos] == open_key)
      return pos;
    else if (rl_line_buffer[pos] == close_key)
      pos = gnu_readline_skip(pos, open_key, close_key);
    else if (rl_line_buffer[pos] == '"')
      pos = gnu_readline_skip(pos, '"', '"');
  }
  return pos;
}

INLINE int
gnu_readline_find_match(char key)
{
  if (key == ')')
    return gnu_readline_skip(rl_point - 1, '(', ')');
  else if (key == ']')
    return gnu_readline_skip(rl_point - 1, '[', ']');
  return 0;
}

// Delays, but returns early if key press occurs
void
gnu_readline_timid_delay(int ms)
{
  struct pollfd pfd;

  pfd.fd = fileno(rl_instream);
  pfd.events = POLLIN || POLLPRI;
  pfd.revents = 0;

  poll(&pfd, 1, ms);
}

// Bounces the cursor to the matching paren for a while
int
gnu_readline_paren_bounce(int count, int key)
{
  int insert_success;
  int old_point;
  int matching;

  if (gnu_readline_bounce_ms == 0)
    return 0;

  // Write the just entered paren out first
  insert_success = rl_insert(count, key);
  if (insert_success != 0)
    return insert_success;
  rl_redisplay();

  // Need at least two chars to bounce...
  if (rl_point < 2) // rl_point set to next char (implicit +1)
    return 0;

  // If it's an escaped paren, don't bounce...
  if (rl_line_buffer[rl_point - 2] == '\\')
    return 0;

  // Bounce
  old_point = rl_point;
  matching = gnu_readline_find_match(key);
  if (matching < 0)
    return 0;
  else
    rl_point = matching;
  rl_redisplay();
  gnu_readline_timid_delay(gnu_readline_bounce_ms);
  rl_point = old_point;

  return 0;
}

#if 0
void paren_match_highlights(char *match_color, char *no_match_color)
{
  paren_colors.with_match = (match_color[0] == '\0'
      ? "\x1b[36m"
      : match_color);
  paren_colors.without_match = (no_match_color[0] == '\0'
      ? "\x1b[35m"
      : no_match_color);
}
#endif

////\\\\//// Tab Completion ////\\\\////

// Prototype for callback into scm
#ifndef RL_EGG_TESTING
C_word
gnu_readline_scm_complete(char *, int, int);

// Gets called (repeatedly) when readline tries to do a completion
// FIXME, I use _strncpy_, but I should probably use _strlcpy_ or _strncat_
char *
gnu_readline_tab_complete(const char *text, int status) {
  C_word result;
  char *str;
  int len;
  char *copied_str;

  /* All of this is for older versions of chicken (< 2.3), which don't
     reliably null-terminate strings */

  // Get scheme string for possible completion via callback
  result = gnu_readline_scm_complete((char *)text, strlen(text), status);

  if (result == C_SCHEME_FALSE)
    return NULL;

  // Convert into C types
  str = C_c_string(result);
  len = C_num_to_int(C_i_string_length(result));

  if (len == 0)
    return NULL;

  // Copy (note: the readline lib frees this copy)
  copied_str = (char *)malloc(len + 1);
  strncpy(copied_str, str, len); /* XXX this probably isn't the best, although it is safe
                                    thanks to the next line, way to copy these strings. */
  copied_str[len] = '\0';
  return copied_str;
}
#endif

int // grants access to the gnu_history_newlines variable.
gnu_history_new_lines()
{
  return gnu_history_newlines;
}

int
gnu_readline_append_history(char *filename)
{
  return append_history(gnu_history_newlines, filename);
}

#if 0
void gnu_readline_highlight_matches()
{
  RL_EGG_DEBUG("match-position: %d\n", find_match(')'));
}
#endif
#if 0
char *gnu_make_arrow_code()
{};
#endif

// Set everything up
#ifndef RL_EGG_TESTING
void
gnu_readline_init()
{
  using_history();
  rl_bind_key(')', gnu_readline_paren_bounce);
  rl_bind_key(']', gnu_readline_paren_bounce);
#if 0
  rl_bind_keyseq("[D", highlight_paren);
#endif
  rl_completion_entry_function = &gnu_readline_tab_complete;
  rl_variable_bind("rl_catch_signals", 0);
  rl_clear_signals();
  rl_set_signals();
  rl_completer_quote_characters = "\"";
}

// Called from scheme to get user input
char * /* TODO rename to `rlegg_readline_line_from_scheme' */
gnu_readline_readline(char *prompt, char *prompt2)
{
  char *empty_prompt;
  int prompt_len;
  HIST_ENTRY *entry;

  if (gnu_readline_buf != NULL) {
    free(gnu_readline_buf);
    gnu_readline_buf = NULL;
  }

  if (0)
    gnu_readline_buf = readline(prompt);
  else
    gnu_readline_buf = readline(prompt2);

  if (gnu_readline_buf != NULL && *gnu_readline_buf != '\0') {
    entry = history_get(history_base + history_length - 1);
    if (entry == NULL || strcmp(entry->line, gnu_readline_buf) != 0) {
      add_history(gnu_readline_buf);
      ++gnu_history_newlines;
    }
  }

  if (strlen(rl_line_buffer) > 0) {
    int adx = quote_in_string(rl_line_buffer);
    _CHKSCM_RL_EGG_DEBUG("quote_in_string: %d\n", adx);
    int bdx = parens_braces_in_string(rl_line_buffer, '(');
    int cdx = parens_braces_in_string(rl_line_buffer, '[');
    balance.quote = (adx == -1 ? 0 : adx);
    if (bdx == -1)
      clear_paren_brace_counts('(');
    if (cdx == -1)
      clear_paren_brace_counts('[');
  }
  return (gnu_readline_buf);
}
#endif

void
gnu_readline_signal_cleanup()
{
  free(gnu_readline_buf);
  gnu_readline_buf = NULL;
  rl_free_line_state();
  rl_cleanup_after_signal();
}

char *
gnu_history_get(int ind, int time)
{
  HIST_ENTRY *entry = NULL;

  entry = history_get(ind);

  if (entry == NULL)
    return NULL;
  return (time == 0 ? entry->line : entry->timestamp);
}

char *
gnu_history_entry(int ind, int time)
{
  HIST_ENTRY *entry = NULL;

  if (ind == 0)
    entry = current_history();
  else if (ind < 0)
    entry = previous_history();
  else if (ind > 0)
    entry = next_history();

  if (entry == NULL)
    return NULL;
  return (time == 0 ? entry->line : entry->timestamp);
}

/* safely concatenates the history_list's entry strings and returns them via a pointer */
char *
gnu_history_list() /* may look a bit messy, but it seems to work great ;D */
{
  HIST_ENTRY **hist_list = history_list();
  char result_buf[BUFSIZ];
  char *result_buf_ptr = result_buf;
  int idx;

  *result_buf_ptr = '\0';

  if (hist_list == NULL)
    return NULL;
  RL_EGG_STRCAT(result_buf, hist_list[0]->line);
  RL_EGG_STRCAT(result_buf, "\n");

  for (idx = 1; idx < history_length; ++idx) {
    RL_EGG_STRCAT(result_buf, hist_list[idx]->line);
    RL_EGG_STRCAT(result_buf, "\n");
  }
  return result_buf_ptr;
}

char *
gnu_history_timeat_current()
{
  HIST_ENTRY *entry = NULL;
  entry = current_history();

  if (entry == NULL || entry->timestamp[0] == '\0')
    return NULL;
  return entry->timestamp;
}

int
gnu_history_list_length()
{
  return history_length;
}
