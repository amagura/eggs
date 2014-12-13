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
#include <string.h>
#include <stdio.h>
#include <sys/poll.h>
#include <readline/readline.h>
#include <readline/history.h>

#if _HAVE_LIBBSD
#include <bsd/string.h>
#endif


static char *gnu_readline_buf = NULL;
static int gnu_readline_bounce_ms = 500;
static int gnu_readline_paren_balance = 0;
static int gnu_readline_brace_balance = 0;
static int gnu_history_newlines = 0;

////\\\\//// Paren Bouncing ////\\\\////

/* Returns: (if positive) the position of the matching paren
            (if negative) the number of unmatched closing parens */
int gnu_readline_skip(int pos, int open_key, int close_key)
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

// Return what the balance is between opening and closing keys
int gnu_readline_match_balance(int open_key, int close_key)
{
  int pos;
  int open = 0;

  // Can't use rl_end intead of strlen: gives length of whole buffer
  pos = gnu_readline_skip(strlen(rl_line_buffer), open_key, close_key);
  if (pos < 0)
    return pos + 1;
  while (pos >= 0) {
    ++open;
    pos = gnu_readline_skip(pos, open_key, close_key);
  }
  return open;
}

// Resets the global vars that track paren balance
void gnu_readline_clear_balances()
{
  gnu_readline_paren_balance = 0;
  gnu_readline_brace_balance = 0;
}


// Finds the matching paren (starting from just left of the cursor)
int gnu_readline_find_match(int key)
{
  if (key == ')')
    return gnu_readline_skip(rl_point - 1, '(', ')');
  else if (key == ']')
    return gnu_readline_skip(rl_point - 1, '[', ']');
  return 0; 
}

// Delays, but returns early if key press occurs 
void gnu_readline_timid_delay(int ms)
{
  struct pollfd pfd;

  pfd.fd = fileno(rl_instream);
  pfd.events = POLLIN || POLLPRI;
  pfd.revents = 0;

  poll(&pfd, 1, ms);
}

// Bounces the cursor to the matching paren for a while
int gnu_readline_paren_bounce(int count, int key)
{
  int insert_success, old_point, matching;
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


////\\\\//// Tab Completion ////\\\\////

// Prototype for callback into scm
C_word gnu_readline_scm_complete(char *, int, int);

// Gets called (repeatedly) when readline tries to do a completion
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
  strncpy(copied_str, str, len);
  copied_str[len] = '\0';

  return copied_str;
}


////\\\\//// Other Stuff ////\\\\////

#if 0
// This is code that is supposed to alter the behaviour of ctrl-w so that it recognizes
// parens as delimiters.  It works, but I can't bind it to ctrl-w, because that key code 
// is intercepted by the terminal (I think).

// Ripped off from readline src
#define emacs_mode 1
#define whitespace_ext(c) ( ((c) == ' ') || ((c) == '\t') || ((c) == '(') || ((c) == '[') )

int gnu_readline_lisp_word_rubout(int count, int key)
{
	int orig_point;

	if (rl_point == 0) {
		rl_ding ();
	} else {
		orig_point = rl_point;
		if (count <= 0)
			count = 1;

		while (count--) {
			while (rl_point && whitespace (rl_line_buffer[rl_point - 1]))
				rl_point--;

			while (rl_point && (whitespace_ext (rl_line_buffer[rl_point - 1]) == 0))
				rl_point--;
		}
		
		if (orig_point == rl_point && 
			(rl_line_buffer[rl_point - 1] == '(' || rl_line_buffer[rl_point - 1] == '['))
			rl_point--;

		rl_kill_text (orig_point, rl_point);
		if (rl_editing_mode == emacs_mode)
			rl_mark = rl_point;
	}

	return 0;
}
#endif

/*  grants access to the gnu_history_newlines variable.
 *  
 *  XXX using a `define-foreign-variable' to gain access to gnu_history_newlines won't work as expected.
 *  I don't _know_ this, but I suspect it's because it only grabs a snapshot of the variable and is not
 *  actually binding itself _to the variable_ 
 */
int gnu_history_new_lines()
{
  return gnu_history_newlines;
}

int gnu_readline_append_history(char *filename)
{
  return append_history(gnu_history_newlines, filename);
}


// Set everything up
void gnu_readline_init()
{
  using_history();
  rl_bind_key(')', gnu_readline_paren_bounce);
  rl_bind_key(']', gnu_readline_paren_bounce);
  rl_completion_entry_function = &gnu_readline_tab_complete;
  rl_variable_bind("rl_catch_signals", 0);
  rl_clear_signals();
  rl_set_signals();
  rl_completer_quote_characters = "\"";

  //rl_add_defun ("lisp-word-rubout" , gnu_readline_lisp_word_rubout, -1);
}


// Called from scheme to get user input
char *gnu_readline_readline(char *prompt, char *prompt2)
{
  char *empty_prompt;
  int prompt_len;
  HIST_ENTRY *h;

  if (gnu_readline_buf != NULL) {
    free(gnu_readline_buf);
    gnu_readline_buf = NULL;
  }

  if ((gnu_readline_paren_balance || gnu_readline_brace_balance) == 0) 
    gnu_readline_buf = readline(prompt);
  else
    gnu_readline_buf = readline(prompt2);

  if (gnu_readline_buf != NULL && *gnu_readline_buf != '\0') {
    h = history_get(history_base + history_length - 1);
    if (NULL == h || 0 != strcmp(h->line, gnu_readline_buf)) {
      add_history(gnu_readline_buf);
      ++gnu_history_newlines;
    }
  }

  gnu_readline_paren_balance += gnu_readline_match_balance('(', ')');
  gnu_readline_brace_balance += gnu_readline_match_balance('[', ']');
  if (gnu_readline_paren_balance < 0 || gnu_readline_brace_balance < 0)
    gnu_readline_clear_balances();

  return(gnu_readline_buf);
}

void gnu_readline_signal_cleanup()
{
    gnu_readline_clear_balances();
    free(gnu_readline_buf);
    gnu_readline_buf = NULL;
    rl_free_line_state();
    rl_cleanup_after_signal();
}

char *gnu_history_lineat_current()
{
  HIST_ENTRY *entry = NULL;
  entry = current_history();

  // did the operation succeed?
  if (entry == NULL)
    return NULL; // no
  return entry->line; // yes
}

int gnu_history_goto_offset(int offset, int relative)
{
  if (relative)
    offset += where_history();

  return history_set_pos(offset);
}

/* gives you the timestamp, if any, for the history entry at the offset in history_list */
char *gnu_history_timeat_offset(int offset)
{
  HIST_ENTRY *entry = NULL;
  char *result = NULL;
  int current_offset = where_history();

  if (gnu_history_goto_offset(offset, 0))
    entry = current_history();

  result = (entry == NULL || entry->timestamp[0] == '\0')
    ? NULL
    : entry->timestamp;

  gnu_history_goto_offset(current_offset, 0);
  return result;
}

/* safely concatenates the history_list's entry strings and returns them via a pointer */
char *gnu_history_list() /* may look a bit messy, but it seems to work great ;D */
{
  HIST_ENTRY **hist_list = history_list();
  char result_buf[BUFSIZ];
  char *result_buf_ptr = result_buf;
  int idx;

#if _HAVE_LIBBSD
#else
  *result_buf = '\0';
#endif

#if _HAVE_LIBBSD
#define _CHK_READLINE_EGG_STRCAT(dst_str, src_str) strlcat(dst_str, src_str, sizeof dst_str)
#define _CHK_READLINE_EGG_STRCPY(dst_str, src_str) strlcpy(dst_str, src_str, sizeof dst_str)
#else
#define _CHK_READLINE_EGG_STRCAT(dst_str, src_str) strncat(dst_str, src_str, (sizeof dst_str - strlen(dst_str) - 1))
#define _CHK_READLINE_EGG_STRCPY(dst_str, src_str) _CHK_READLINE_EGG_STRCAT(dst_str, src_str)
#endif

  if (hist_list == NULL)
    return NULL;

  _CHK_READLINE_EGG_STRCPY(result_buf, hist_list[0]->line);
  _CHK_READLINE_EGG_STRCAT(result_buf, "\n");

  for (idx = 1; idx < history_length; ++idx) {
    _CHK_READLINE_EGG_STRCAT(result_buf, hist_list[idx]->line);
    _CHK_READLINE_EGG_STRCAT(result_buf, "\n");
  }
  return result_buf_ptr;
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
