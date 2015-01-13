// (Readline is GPLed, so that makes this file GPLed too, because this
// file will only run if it's linked against readline.)
//
// Copyright (c) 2002 Tony Garnock-Jones
// Copyright (c) 2006 Heath Johns (paren bouncing and auto-completion code)
// Copyright (c) 2015 Alexej Magura (added a lot of history functions and more)
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

#if _HAVE_LIBBSD // then we can use strlcat and strlcpy
#include <bsd/string.h>
#define _CHKSCM_RL_EGG_STRCAT(_CHKSCM_RL_EGG__DST, _CHKSCM_RL_EGG__SRC) \
  strlcat((_CHKSCM_RL_EGG__DST), (_CHKSCM_RL_EGG__SRC), sizeof (_CHKSCM_RL_EGG__DST))
//#define _CHKSCM_RL_EGG_STRCPY(dst_str, src_str) strlcpy(dst_str, src_str, sizeof dst_str)
#define _CHKSCM_RL_EGG_STRCAT_FN_USED "strlcat"
#else
#define _CHKSCM_RL_EGG_STRCAT(_CHKSCM_RL_EGG__DST, _CHKSCM_RL_EGG__SRC) \
  strncat((_CHKSCM_RL_EGG__DST), (_CHKSCM_RL_EGG__SRC), (sizeof (_CHKSCM_RL_EGG__DST) - strlen((_CHKSCM_RL_EGG__DST)) - 1))
#define _CHKSCM_RL_EGG_STRCAT_FN_USED "strncat"
#endif

/*@ignore@*/
#if 0 // NOTE change this to 1 to enable debug messages
#define _CHKSCM_RL_EGG_DEBUG(format, ...) fprintf(stderr, format, __VA_ARGS__)
#define _CHKSCM_RL_EGG_DEBUG_LIT(format, ...) fprintf(stderr, format, #__VA_ARGS__)
#else
#define _CHKSCM_RL_EGG_DEBUG(format, ...)
#define _CHKSCM_RL_EGG_DEBUG_LIT(format, ...)
#endif
/*@end@*/

struct total_t {
  int open;
  int close;
};

struct delim_t {
  int open;
  int close;
};

struct key_balance_t {
  int start_pos;
  int end_pos;
  int total[2]; // 0 -> open, 1 -> close
};

struct key_t {
  struct count_t count;
  char token[2];
};

struct balance_t {
  struct key_t parens;
  struct key_t braces;
  int dbl_quotes;
} balance;

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
   (gnu-readline-parse-and-bind "set blink-matching-paren on")

   XXX it however does ____NOT____ work.  built-in: (line 'a) ')
                                                    ^ -------- ^ ; WRONG
   ~ Alexej
*/
inline char
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

inline int // returns -1 when stringp points to a string containing only NULL
excess_open_delim(char *stringp, const char open_delim, const char close_delim)
{
  int open = 0;
  int close = 0;
  char *cp = &stringp[strlen(stringp)];

  if (cp == stringp)
    return -1;

  do {
    if (cp != stringp && peek_chr(cp, stringp, false) == '\\')
      continue;

    if (*cp == close_delim)
      ++close;
    else if (*cp == open_delim)
      ++open;
  } while(cp-- != stringp);

  if (open_delim == close_delim && close > 0) {
    if (close % 2 == 1)
      while(open++ < --close);
    else
      close %= 2;
  }
  return open - close;
}

inline struct key_t
clear_key_counts(struct key_t keys)
{
  int idx = 0;
  for (; idx < 3; ++idx) {
    keys.counts[idx] = 0;
  }
  return keys;
}

inline struct balance_t
key_balance_in_string(char *string, char open_key, char close_key)
{
  struct key_t keys = count_key_in_string(string, open_key, close_key);
  keys.counts[0] =


inline struct key_t
count_key_in_string(char *string, char open_key, char close_key)
{
  struct key_t keys;
  keys.count[0] = 0;
  keys.count[1] = 0;
  keys.count[2] = 0;
  int *idxp = keys.counts;
  char *char_ptr = strend_addr(string);
  int dbl_quote_balance = (dbl_quotes_in_string(string) % 2);
  keys.tokens[0] = open_key;
  keys.tokens[1] = close_key;

  if (char_ptr == string)
    return keys;

  do {
    if (char_ptr != string && prev_char(char_ptr, string) == '\\')
      continue;

    _CHKSCM_RL_EGG_DEBUG("balance.quote: %d\n", dbl_quote_balance);
    if (*char_ptr == open_key && dbl_quote_balance < 1) { // checks to make sure we are not in a quoted string
      ++(*idxp); // increment total
      ++(*(++idxp)); // move to open, and increment
      --idxp; // move back to total
    } else if (*char_ptr == close_key && dbl_quote_balance < 1) {
      --(*idxp); // deincrement total
      ++idxp; // move to open
      ++(*(++idxp)); // move to close and increment
      --idxp; // move back to open
      --idxp; // move back to total
    }
  } while (char_ptr-- != string);

  return keys;
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
inline int
matching_key_pos(char *key_location, const char open_key, const char close_key)
{
  if (pos == 0)
    return NULL;

  if (open_key == '"') {




  do {
    if (pos > 0 && rl_line_buffer[pos - 1] == '\\') {
      continue;

    if (rl_line_buffer[pos] == open_key && ) {




  while (char_ptr) {

  do {
    if (

  while (--pos > -1) {
    if (pos > 0 && rl_line_buffer[pos - 1] == '\\') {
      continue;
    } else if (rl_line_buffer[pos] == open_key) {
      return pos;
    } else if (rl_line_buffer[pos] == close_key) {
      pos = gnu_readline_skip(pos, open_key, close_key); // FIXME, I'm a recurisve call
    } else if (rl_line_buffer[pos] == '"') {
      pos = gnu_readline_skip(pos, '"', '"'); // FIXME, I'm a recursive call
    }
  }
  return pos;
}

// Finds the matching paren (starting from just left of the cursor)
inline int
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

/*  grants access to the gnu_history_newlines variable.
 *
 *  XXX using a `define-foreign-variable' to gain access to gnu_history_newlines won't work as expected.
 *  I don't _know_ this, but I suspect it's because it only grabs a snapshot of the variable and is not
 *  actually binding itself _to the variable_
 */
int
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
  _CHKSCM_RL_EGG_DEBUG("match-position: %d\n", find_match(')'));
}
#endif
#if 0
char *gnu_make_arrow_code()
{};
#endif

// Set everything up
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

  if (!(balance[0] || balance[1] || balance[2]))
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

void
gnu_readline_signal_cleanup()
{
  balance.quote = 0;
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
  _CHKSCM_RL_EGG_DEBUG("_CHKSCM_RL_EGG_STRCAT: %s\n", _CHKSCM_RL_EGG_STRCAT_FN_USED);
  _CHKSCM_RL_EGG_STRCAT(result_buf, hist_list[0]->line);
  _CHKSCM_RL_EGG_STRCAT(result_buf, "\n");

  for (idx = 1; idx < history_length; ++idx) {
    _CHKSCM_RL_EGG_STRCAT(result_buf, hist_list[idx]->line);
    _CHKSCM_RL_EGG_STRCAT(result_buf, "\n");
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
