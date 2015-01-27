// (Readline is GPLed, so that makes this file GPLed too, because this
// file will only run if it's linked against readline.)
//
// Copyright (c) 2002 Tony Garnock-Jones
// Copyright (c) 2006 Heath Johns (paren bouncing and auto-completion code)
// Copyright (c) 2014 Alexej Magura (added a lot of history functions and more)
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
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/poll.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <stdbool.h>
#include <errno.h>
#include <alloca.h>

/* **** BEGIN ****
 * Macros *
 */

#define RL_EGG_MEMSAME(RL_EGG_MEM_1, RL_EGG_MEM_2)	\
  (((RL_EGG_MEM_1) == (RL_EGG_MEM_2) && 1) || 0)
#define RL_EGG_BOOLSTR(RL_EGG_BOOL)		\
  (((RL_EGG_BOOL) && "true") || "false")
#define RL_EGG_NULLCASE(RL_EGG_CHECKED, RL_EGG_DEFAULT)	\
  ((RL_EGG_CHECKED) || (RL_EGG_DEFAULT))
/* *** BEGIN ***
 * Conditional Macros *
 */
#ifndef INLINE
#if defined __GNUC__
#define INLINE __inline__
#else
#define INLINE inline
#endif
#endif

#ifndef DEBUG
#define DEBUG 1
#endif

#if DEBUG /* NOTE change this to 1 to enable debug messages */
#define RL_EGG_BEGIN_TRACE fprintf(stderr, "++ %s{%d}:\n\n", __FILE__, __LINE__)
#define RL_EGG_END_TRACE fprintf(stderr, "\n-- %s %s `%s'\n\n", "In", "function", __FUNCTION__)
#define RL_EGG_DEBUG(format, ...)		\
  do {						\
    fprintf(stderr, "%s", "\e[36m");		\
    fprintf(stderr, (format), __VA_ARGS__);	\
    fprintf(stderr, "%s", "\e[0m");		\
  } while(0)
#else
#define RL_EGG_BEGIN_TRACE
#define RL_EGG_END_TRACE
#define RL_EGG_DEBUG(format, ...)
#endif

#if HAVE_LIBBSD /* then we can use strlcat */
#include <bsd/string.h>
#define RL_EGG_STRCAT(DESTINATION_STR, SOURCE_STR, DESTINATION_SIZE)	\
  strlcat((DESTINATION_STR), (SOURCE_STR), sizeof (DESTINATION_SIZE))
#define RL_EGG_STRCAT_FN_USED "strlcat"
#else /* else we have to use strncat */
#define RL_EGG_STRCAT(DESTINATION_STR, SOURCE_STR, DESTINATION_SIZE)	\
  strncat((DESTINATION_STR), (SOURCE_STR), ((DESTINATION_SIZE) - strlen(DESTINATION_STR) - 1))
#define RL_EGG_STRCAT_FN_USED "strncat"
#endif
/* *** END ***
   Macros
 * *** END ***
 */

/* *** BEGIN ***
   Globals
 * *** BEGIN ***
 */
struct delim {
  char open_delim;
  char close_delim;
  int open;
  int close;
};

static int gnu_readline_bounce_ms = 500;
static int gnu_history_newlines = 0;
static char *gnu_readline_buf = NULL;

#if 0 // NOT YET IMPLEMENTED
static struct gnu_readline_current_paren_color_t {
  int enabled;
  char *with_match;
  char *without_match;
} paren_colors;
#endif

/* XXX readline already provides paren-bouncing:
 *  (gnu-readline-parse-and-bind "set blink-matching-paren on")
 *
 *  XXX it however does ____NOT____ work.  built-in: (line 'a) ')
 *                                                   ^ -------- ^ ; WRONG
 *  ~ Alexej
 */

/* *** END ***
   Globals
 * *** END ***
 */

/* *** BEGIN ***
   Inline Functions
 */
INLINE char peek_chr(char *cp, char *stringp, bool direct)
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

INLINE int strnof_chr(char *str, const char chr)
{
  char *cp = &str[strlen(str)];
  int count = 0;

  if (cp == str)
    return -1;

  do {
    if (cp != str && peek_chr(cp, str, false) == '\\')
      continue;

    if (*cp == chr)
      ++count;

  } while(cp-- != str);
  return count;
}

INLINE int strnof_delim(char *str, struct delim *info)
{
  char *cp = &str[strlen(str)];
  info->open = 0;
  info->close = 0;

  RL_EGG_DEBUG("open: %d\nclose: %d\n", info->open, info->open);

  if (cp == str)
    return 1;

  do {
    if (cp != str && peek_chr(cp, str, false) == '\\')
      continue;

    if (*cp == info->close_delim) {
      ++info->close;
    } else if (*cp == info->open_delim) {
      ++info->open; // increment open
    }
  } while(cp-- != str);

  if (info->open_delim == info->close_delim && info->close > 0) {
    if (info->close % 2 == 1)
      while(info->open++ < --info->close);
    else {
      info->open = info->close * 0.5;
      info->close *= 0.5;
    }
  }

  RL_EGG_BEGIN_TRACE;
  RL_EGG_DEBUG("open: %d\nclose: %d\n", info->open, info->close);
  RL_EGG_END_TRACE;

  return 0;
}

/* sets `buf' to the unquoted portions of `str'
 * returns:
 * (*) 0 (on success)
 * (*) -1 if `str', has no quoted portions, leaving `buf' untouched.
 * (*) `errno' on error
 */
INLINE int str_nquotd(char *str, char *buf, const size_t bufsize)
{
  struct delim info;
  //char *token, *tmp;
  //int even;
  int result = -1;
  info.open_delim = '"';
  info.close_delim = '"';
  strnof_delim(str, &info);
  RL_EGG_DEBUG("idx[0]: %d\n", info.open);
  RL_EGG_DEBUG("idx[1]: %d\n", info.close);

  if (info.open == 0)
    goto end;
  int even = info.open - abs(info.open - info.close);
  char *token, *tmp, *rest;
  token = NULL;
  tmp = NULL;
  rest = NULL;
  tmp = strdupa(str);

  if (tmp == NULL) {
#if DEBUG
    perror(__FUNCTION__);
#endif
    result = errno;
    goto end;
  }

  token = strtok_r(tmp, "\"", &rest);
  RL_EGG_DEBUG("token: %s\n", token);
  RL_EGG_STRCAT(buf, token, bufsize);

  while ((token = strtok_r(NULL, "\"", &rest)) != NULL) {
    RL_EGG_DEBUG("token (while): %s\n", token);
    RL_EGG_DEBUG("even (while): %d\n", even);
    if (even % 2 == 1) {
      RL_EGG_STRCAT(buf, token, bufsize);
      --even;
    } else {
      ++even;
    }
  }
  ++result;
 end:
  return result;
}

#if 0 // NOT YET IMPLEMENTED
int
highlight_paren()
{
  char *rl_ptr = rl_line_buffer;
  while (rl_ptr != &rl_line_buffer[rl_point]) { ++rl_ptr; }
  RL_EGG_DEBUG("%s\n", rl_ptr);
  return 0;
}
#endif

////\\\\//// Paren Bouncing ////\\\\////

/* Returns: (if positive) the position of the matching paren
            (if negative) the number of unmatched closing parens */
INLINE int gnu_readline_skip(int pos, char open_key, char close_key)
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

// Finds the matching paren (starting from just left of the cursor)
INLINE int gnu_readline_find_match(char key)
{
  if (key == ')')
    return gnu_readline_skip(rl_point - 1, '(', ')');
  else if (key == ']')
    return gnu_readline_skip(rl_point - 1, '[', ']');
  return 0;
}

// Delays, but returns early if key press occurs
INLINE void gnu_readline_timid_delay(int ms)
{
  struct pollfd pfd;

  pfd.fd = fileno(rl_instream);
  pfd.events = POLLIN || POLLPRI;
  pfd.revents = 0;

  poll(&pfd, 1, ms);
}
/* *** END ***
 * Inline Functions
 */

int track_balnc(const char open_delim, const int unbal)
{
  static int braces = 0;
  static int parens = 0;
  static int dquotes = 0;
  if (open_delim == '(') {
    parens += unbal;
    return parens;
  } else if (open_delim == '[') {
    braces += unbal;
    return braces;
  } else {
    dquotes += unbal;
    return dquotes;
  }
}

INLINE void reset_prompt_balance()
{

  int balnc = track_balnc('"', 0);
  track_balnc('"', -balnc);
  balnc = track_balnc('(', 0);
  track_balnc('(', -balnc);
  balnc = track_balnc('[', 0);
  track_balnc('[', -balnc);
}

INLINE bool check_prompt_balance(char *str)
{
  bool result = 1;
  char *buf = alloca(RL_EGG_NULLCASE(rl_end, strlen(str)));
  int has_quotes = str_nquotd(str, buf, RL_EGG_NULLCASE(rl_end, strlen(str)));
  int idx;

  struct delim info;
  for (idx = 0; idx < 2; ++idx) {
    if (result != 1)
      break;
    if (idx == 1) {
      RL_EGG_DEBUG("%s\n", "ptr is brace");
      info.open_delim = '[';
      info.close_delim = ']';
       } else if (idx < 1) {
      RL_EGG_DEBUG("%s\n", "ptr is paren");
      info.open_delim = '(';
      info.close_delim = ')';
      } else {
      RL_EGG_DEBUG("%s\n", "ptr is dquote");
      info.open_delim = '"';
      info.close_delim = '"';
    }
    if (info.open_delim != '"') {
      if (has_quotes == -1)
	strnof_delim(str, &info);
      else
	strnof_delim(buf, &info);
    }
    (track_balnc(info.open_delim, info.open - info.close) % 2) == 0 || --result;
  }
  return (bool)result;
}


// Bounces the cursor to the matching paren for a while
int gnu_readline_paren_bounce(int count, int key)
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
C_word gnu_readline_scm_complete(char *, int, int);

// Gets called (repeatedly) when readline tries to do a completion
// FIXME, I use _strncpy_, but I should probably use _strlcpy_ or _strncat_
char *gnu_readline_tab_complete(const char *text, int status) {
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

// grants access to the gnu_history_newlines variable.
int gnu_history_new_lines()
{
  return gnu_history_newlines;
}

int gnu_readline_append_history(char *filename)
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
void gnu_readline_init()
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
char *gnu_readline_readline(char *prompt, char *prompt2)
{
  char *empty_prompt;
  int prompt_len;
  HIST_ENTRY *entry;

  if (gnu_readline_buf != NULL) {
    free(gnu_readline_buf);
    gnu_readline_buf = NULL;
  }

  if (check_prompt_balance(rl_line_buffer))
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

#if 0
  if (strlen(rl_line_buffer) > 0) {
    int adx = quote_in_string(rl_line_buffer);
    RL_EGG_DEBUG("quote_in_string: %d\n", adx);
    int bdx = parens_braces_in_string(rl_line_buffer, '(');
    int cdx = parens_braces_in_string(rl_line_buffer, '[');
    balance.quote = (adx == -1 ? 0 : adx);
    if (bdx == -1)
      clear_paren_brace_counts('(');
    if (cdx == -1)
      clear_paren_brace_counts('[');
  }
#endif
  return (gnu_readline_buf);
}
#endif

void gnu_readline_signal_cleanup()
{
  reset_prompt_balance();
  free(gnu_readline_buf);
  gnu_readline_buf = NULL;
  rl_free_line_state();
  rl_cleanup_after_signal();
}

char *gnu_history_get(int ind, int time)
{
  HIST_ENTRY *entry = NULL;

  entry = history_get(ind);

  if (entry == NULL)
    return NULL;
  return (time == 0 ? entry->line : entry->timestamp);
}

char *gnu_history_entry(int ind, int time)
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
char *gnu_history_list() /* may look a bit messy, but it seems to work great ;D */
{
  const size_t bufsize = history_total_bytes();
  char *buf = malloc(bufsize+1);
  HIST_ENTRY **hist_list = history_list();
  int idx;

  if (hist_list == NULL)
    return NULL;
  RL_EGG_DEBUG("RL_EGG_STRCAT: %s\n", RL_EGG_STRCAT_FN_USED);
  RL_EGG_STRCAT(buf, hist_list[0]->line, bufsize);
  RL_EGG_STRCAT(buf, "\n", bufsize);

  for (idx = 1; idx < history_length; ++idx) {
    RL_EGG_STRCAT(buf, hist_list[idx]->line, bufsize);
    RL_EGG_STRCAT(buf, "\n", bufsize);
  }
  return buf;
}

int gnu_history_list_max_length()
{
  return history_max_entries;
}

char *gnu_history_timeat_current()
{
  HIST_ENTRY *entry = NULL;
  entry = current_history();

  if (entry == NULL || entry->timestamp[0] == '\0')
    return NULL;
  return entry->timestamp;
}

int gnu_history_list_length()
{
  return history_length;
}
