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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/poll.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <stdbool.h>

#if _HAVE_LIBBSD // then we can use strlcat and strlcpy
#include <bsd/string.h>
#define _CHK_READLINE_EGG_STRCAT(dst_str, src_str) strlcat((dst_str), (src_str), sizeof (dst_str))
//#define _CHK_READLINE_EGG_STRCPY(dst_str, src_str) strlcpy(dst_str, src_str, sizeof dst_str)
#define _CHK_READLINE_EGG_STRCPY(dst_str, src_str) _CHK_READLINE_EGG_STRCAT(dst_str, src_str)

#define _CHK_READLINE_EGG_STRCAT_FN_USED "strlcat"
#define _CHK_READLINE_EGG_STRCPY_FN_USED _CHK_READLINE_EGG_STRCAT_FN_USED
#else
#define _CHK_READLINE_EGG_STRCAT(dst_str, src_str) strncat(dst_str, src_str, (sizeof dst_str - strlen(dst_str) - 1))
#define _CHK_READLINE_EGG_STRCPY(dst_str, src_str) _CHK_READLINE_EGG_STRCAT(dst_str, src_str)

#define _CHK_READLINE_EGG_STRCAT_FN_USED "strncat"
#define _CHK_READLINE_EGG_STRCPY_FN_USED _CHK_READLINE_EGG_STRCAT_FN_USED
#endif

#define _CHK_READLINE_EGG_INCR_ONTOK(token) \
  do { \
    switch ((token)) { \
      case '(': \
                ++balance.paren.open; \
      break; \
      case ')': \
                ++balance.paren.close; \
      break; \
      case '[': \
                ++balance.brace.open; \
      break; \
      case ']': \
                ++balance.brace.close; \
      break; \
      case '"': \
                charc % 2 == 0 || ++balance.quote; \
      break; \
    } \
  } while(0)

#define _CHK_READLINE_EGG_SETPROP_3(prop1, prop2, prop3, val) (prop1) = (val); (prop2) = (val); (prop3) = (val)

#define _CHK_READLINE_EGG_CLEAR_SUB(sub_struct) \
  do { \
    (sub_struct).close = 0; \
    (sub_struct).open = 0; \
  } while(0)

#define _CHK_READLINE_EGG_OR_3(val1, val2, val3) ((val1) || (val2) || (val3))
#define _CHK_READLINE_EGG_NOR_3(val1, val2, val3) !(_CHK_READLINE_EGG_OR_3((val1), (val2), (val3)))

#define _CHK_READLINE_EGG_IF_THEN_SET(exp, sub_struct, val) \
  do { \
    if ((exp)) { \
      (sub_struct).open = (val); \
      (sub_struct).close = (val); \
    } \
  } while(0)

#if 0 // NOTE change this to 1 to enable debug messages
#define _CHK_READLINE_EGG_DEBUG(format, ...) fprintf(stderr, format, __VA_ARGS__)
#define _CHK_READLINE_EGG_DEBUG_LIT(format, ...) fprintf(stderr, format, #__VA_ARGS__)
#else
#define _CHK_READLINE_EGG_DEBUG(format, ...)
#define _CHK_READLINE_EGG_DEBUG_LIT(format, ...)
#endif

struct balance_t {
  int paren[3]; // 0 -> total, 1 -> open, 2 -> close
  int brace[3]; // 0 -> total, 1 -> open, 2 -> close
  int quote;
} balance;

static char *gnu_readline_buf = NULL;
static int gnu_readline_paren_balance = 0;
static int gnu_readline_bounce_ms = 500;
static int gnu_history_newlines = 0;
#if 0// NOT YET IMPLEMENTED
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
// >>>1
inline char *
strtail_addr(const char *string)
{
  return (char *)&string[strlen(string) - 1];
}

inline char *
strend_addr(const char *string)
{
  return (char *)&string[strlen(string)];
}

inline char
prev_char(char *stringp, char *string)
{
  char result = '\0';
  if (stringp != string) {
    --stringp;
    result = *stringp;
    ++stringp;
  } else {
    result = *stringp;
  }
  return result;
}

inline bool
ptr_strhead(char *stringp, char *string)
{
  return stringp == string;
}

inline bool
ptr_not_strhead(char *stringp, char *string)
{
  return !ptr_strhead(stringp, string);
}
// <<<1
inline int /* working */
quote_in_string(char *string)
{
  char *str_ptr = strend_addr(string);

  if (str_ptr == string)
    return 0;

  do {
    if (ptr_not_strhead(str_ptr, string) && prev_char(str_ptr, string) == '\\')
      continue;

    if (*str_ptr == '"') {
      ++balance.quote;
    }
  } while (str_ptr-- != string);

  if (balance.quote == 0)
    return -1;
  _CHK_READLINE_EGG_DEBUG("return balance.quote: %d\n", balance.quote);
  return balance.quote % 2;
}

inline void
clear_parbar(char token)
{
  int idx = 0;
  for (; idx < 3; ++idx) {
    if (token == '(' || token == ')')
      balance.paren[idx] = 0;
    if (token == '[' || token == ']')
      balance.brace[idx] = 0;
  }
}

inline int /* not working */
parbar_in_string(char *string, char add_token)
{
  int *idxp = NULL;
  char sub_token = '\0';
  if (add_token == '(') {
    idxp = balance.paren;
    sub_token = ')';
  } else if (add_token == '[') {
    idxp = balance.brace;
    sub_token = ']';
  }

  char *str_ptr = strend_addr(string);

  if (str_ptr == string)
    return 0;

  do {
    if (ptr_not_strhead(str_ptr, string) && prev_char(str_ptr, string) == '\\')
      continue;

    _CHK_READLINE_EGG_DEBUG("balance.quote: %d\n", balance.quote);
    if (*str_ptr == add_token && balance.quote < 1) {
      ++(*idxp); // increment total
      ++(*(++idxp)); // move to open, and increment
      --idxp; // move back to total
    } else if (*str_ptr == sub_token && balance.quote < 1) {
      --(*idxp); // deincrement total
      ++idxp; // move to open
      ++(*(++idxp)); // move to close and increment
      --idxp; // move back to open
      --idxp; // move back to total
    }
  } while (str_ptr-- != string);
  return *idxp;
}

int
highlight_paren()
{
  char *rl_ptr = rl_line_buffer;
  while (rl_ptr != &rl_line_buffer[rl_point]) { ++rl_ptr; }
  _CHK_READLINE_EGG_DEBUG("%s\n", rl_ptr);
  return 0;
}

////\\\\//// Paren Bouncing ////\\\\////

/* Returns: (if positive) the position of the matching paren
            (if negative) the number of unmatched closing parens */
int
gnu_readline_skip(int pos, char open_key, char close_key)
{
  while (--pos > -1) {
    if (pos > 0 && rl_line_buffer[pos - 1] == '\\') {
      continue;
    } else if (rl_line_buffer[pos] == open_key) {
      return pos;
    } else if (rl_line_buffer[pos] == close_key) {
      pos = gnu_readline_skip(pos, open_key, close_key);
    } else if (rl_line_buffer[pos] == '"') {
      pos = gnu_readline_skip(pos, '"', '"');
    }
  }
  return pos;
}

// Finds the matching paren (starting from just left of the cursor)
int
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
  _CHK_READLINE_EGG_DEBUG("match-position: %d\n", find_match(')'));
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
char *
gnu_readline_readline(char *prompt, char *prompt2)
{
  char *empty_prompt;
  int prompt_len;
  HIST_ENTRY *entry;

  if (gnu_readline_buf != NULL) {
    free(gnu_readline_buf);
    gnu_readline_buf = NULL;
  }
  
  if (!(balance.quote || balance.paren[0] || balance.brace[0]))
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
    _CHK_READLINE_EGG_DEBUG("quote_in_string: %d\n", adx);
    int bdx = parbar_in_string(rl_line_buffer, '(');
    int cdx = parbar_in_string(rl_line_buffer, '[');
    balance.quote = (adx == -1 ? 0 : adx);
    if (bdx == -1)
      clear_parbar('(');
    if (cdx == -1)
      clear_parbar('[');
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

  *result_buf = '\0';

  if (hist_list == NULL)
    return NULL;
  _CHK_READLINE_EGG_DEBUG("_CHK_READLINE_EGG_STRCPY: %s\n", _CHK_READLINE_EGG_STRCPY_FN_USED);
  _CHK_READLINE_EGG_DEBUG("_CHK_READLINE_EGG_STRCAT: %s\n", _CHK_READLINE_EGG_STRCAT_FN_USED);
  _CHK_READLINE_EGG_STRCPY(result_buf, hist_list[0]->line);
  _CHK_READLINE_EGG_STRCAT(result_buf, "\n");

  for (idx = 1; idx < history_length; ++idx) {
    _CHK_READLINE_EGG_STRCAT(result_buf, hist_list[idx]->line);
    _CHK_READLINE_EGG_STRCAT(result_buf, "\n");
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
