#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#ifdef HAVE_LIBBSD_STRING_H
#include <bsd/string.h>
#endif

static char *rl_line_buffer = "";

struct balance_t {
  int paren[3]; // 0 -> total parens, 1 -> open, 2 -> close
  int brace[3]; // 0 -> total braces, 1 -> open, 2 -> close
  int quote;
} balance;

#if 1
#define _CHK_READLINE_EGG_DEBUG(format, ...) fprintf(stderr, format, __VA_ARGS__)
#define _CHK_READLINE_EGG_DEBUG_LIT(format, ...) fprintf(stderr, format, #__VA_ARGS__)
#else
#define _CHK_READLINE_EGG_DEBUG(format, ...)
#define _CHK_READLINE_EGG_DEBUG_LIT(format, ...)
#endif

int skip(int pos, int open_key, int close_key)
{
  while (--pos > -1) {
    if (pos > 0 && rl_line_buffer[pos - 1] == '\\') {
      continue;
    } else if (rl_line_buffer[pos] == open_key) {
      return pos;
    } else if (rl_line_buffer[pos] == close_key) {
      pos = skip(pos, open_key, close_key);
    } else if (rl_line_buffer[pos] == '"') {
      pos = skip(pos, '"', '"');
    }
  }
  return pos;
}

// Return what the balance is between opening and closing keys
int match_balance(int open_key, int close_key)
{
  int pos;
  int open = 0;

  // Can't use rl_end intead of strlen: gives length of whole buffer
  pos = skip(strlen(rl_line_buffer), open_key, close_key);
  if (pos < 0)
    return pos + 1;

  while (pos >= 0) {
    ++open;
    pos = skip(pos, open_key, close_key);
  }

  return open;
}

//>>>1
inline char * strtail_addr(const char *string)
{
  return (char *)&string[strlen(string) - 1];
}

inline char * strend_addr(const char *string)
{
  return (char *)&string[strlen(string)];
}

inline bool char_is_quote(char _char, int count)
{
  return (_char == '"') && (count % 2 == 0);
}

inline bool ptr_strhead(char *ptr, char *string)
{
  return ptr == string;
}

inline bool ptr_not_strhead(char *ptr, char *string)
{
  return !ptr_strhead(ptr, string);
}

inline char prev_char(char *ptr, char *string)
{
  char result = '\0';
  if (ptr != string) {
    --ptr;
    result = *ptr;
    ++ptr;
  } else {
    result = *ptr;
  }
  return result;
}
//<<<1

inline void
init_balance()
{
  int idx = 0;
  for (; idx < 3; ++idx) {
    balance.brace[idx] = 0;
    balance.paren[idx] = 0;
  }
  balance.quote = 0;
}

#if 0
inline int char_balance_in_string(char *string, char add_token, char sub_token)
{
    return 0;

  while (char_ptr != &string[0] && --char_ptr) {
    //_CHK_READLINE_EGG_DEBUG("*char_ptr: %c\n", *char_ptr);
    if (ptr_not_strhead(char_ptr, string) && prev_charstr(char_ptr, string) == '\\') // XXX here we increment so as to undo the effect of prior deincrement
      continue;

    if (*char_ptr == add_token && (ptr_strhead(char_ptr, string) || (ptr_not_strhead(char_ptr, string) && *(--char_ptr) != '"')) && balance.quote < 1) {
      //_CHK_READLINE_EGG_DEBUG("found add_token: %c\n", add_token);
      ++charc;
      _CHK_READLINE_EGG_DEBUG("charc add_token: %d\n", charc);
      switch (add_token) {
        case '(':
          ++balance.paren.open;
          break;
        case '[':
          ++balance.brace.open;
          break;
        case '"':
          charc % 2 == 0 || ++balance.quote;
          break;
      }
    }
    else if (*char_ptr == sub_token) {
      --charc;
      _CHK_READLINE_EGG_DEBUG("charc sub_token: %d\n", charc);
      switch (sub_token) {
        case ')':
          ++balance.paren.close;
          break;
        case ']':
          ++balance.brace.close;
          break;
        case '"':
          charc % 2 == 0 || ++balance.quote;
          break;
      }
    }
  }
  return charc;
}

int check_balance(char open_key)
{
  int rl_length = strlen(rl_line_buffer);
  char close_key = (open_key == '('
      ? ')'
      : (open_key == '['
        ? ']'
        : '"'));

  int balanced = key_balance(rl_length, open_key, close_key);
  switch (open_key) {
    case '(':
      (balanced == 0 || balanced == -1 || balanced % 2 == 0) || ++balance.paren.open;
      break;
    case '[':
      (balanced == 0 || balanced == -1 || balanced % 2 == 0) || ++balance.brace.open;
      break;
    case '"':
      (balanced == 0 || balanced == -1 || balanced % 2 == 0) || ++balance.quote; /* this extra check keeps `" "` from making the
                                                                                second "unclosed expression" prompt from appearing. */
      break;
  }
  return balanced;
}
#endif

struct count_t {
  int open;
  int close;
};

struct count_t
count_delim(char *stringp, char open_key, char close_key) {
  struct count_t count;
  char *cp = &stringp[strlen(stringp)];

  do {
    if (cp != stringp && prev_char(cp, stringp) == '\\')
      continue;

    if (*cp == close_key) {
      ++count.close;
    } else if (*cp == open_key) {
      ++count.open;
    }

  } while (cp-- != stringp);

  if (open_key == close_key) {
    if (count.close % 2 == 1) {
     while (count.open++ < --count.close);
    } else {
      count.open = count.close / 2;
      count.close /= 2;
    }
  }

  return count;
}

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

char *
matching_delim_pos(char *stringp, char open_key, char close_key)
{
  char *cp = &stringp[strlen(stringp)];

  do {

  } while(0);
}






int main()
{

  char string[BUFSIZ] = "{{{{{helloa} } } } ";
  printf("excess open: %d\n", excess_open_delim(string, '{', '}'));
  char *tmp;
  tmp = strtok(string, "\"");
  printf("%s\n", tmp);
  tmp = strtok(NULL, "\"");
  printf("%s\n", tmp);
#if 0
  init_balance();

  checks("\"hello");
  checks("\"");
  checks("\"");
  checks("\" \"");
  checks("\"\"");
  checks("(hello");
  checks("((");
  checks(")");
  checks("(");
  checks(")");
  checks(")");
  checks("\(");
#endif
  return 0;
}
