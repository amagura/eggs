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

inline char prev_charstr(char *ptr, char *string)
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

inline int
token_in_string(char *string, char add_token)
{
  int *idxp = NULL;
  char sub_token = '\0';
  if (add_token == '(') {
    idxp = balance.paren;
    sub_token = ')';
  } else {
    idxp = balance.brace;
    sub_token = ']';
  }

  char *str_ptr = strend_addr(string);

  if (str_ptr == string)
    return 0;
  do {
    if (ptr_not_strhead(str_ptr, string) && prev_charstr(str_ptr, string) == '\\')
      continue;

    if (*str_ptr == add_token) {
      ++(*idxp); // increment total
      ++(*(++idxp)); // move to open, and increment
      --idxp; // move back to total
    } else if (*str_ptr == sub_token) {
      --(*idxp); // deincrement total
      ++idxp; // move to open
      ++(*(++idxp)); // move to close and increment
      --idxp; // move back to open
      --idxp; // move back to total
    }
  } while (str_ptr != string && --str_ptr);
  return *idxp; // return total
}

inline int
quote_in_string(char *string)
{
  char *str_ptr = strend_addr(string);

  if (str_ptr == string)
    return 0;

  do {
    if (ptr_not_strhead(str_ptr, string) && prev_charstr(str_ptr, string) == '\\')
      continue;

    if (*str_ptr == '"') {
      ++balance.quote; // increment balance.quote
    }
  } while (str_ptr-- != string);

  if (balance.quote == 0)
    return -1;
  _CHK_READLINE_EGG_DEBUG("return balance.quote: %d\n", balance.quote);
  balance.quote %= 2;

  return balance.quote;
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

inline void
print_balance()
{
  int idx = 0;
  fprintf(stderr, "\n");
  for (; idx < 3; ++idx) {
    fprintf(stderr, "balance.paren[%d]: %d\n", idx, balance.paren[idx]);
  }
  for (idx = 0; idx < 3; ++idx) {
    fprintf(stderr, "balance.brace[%d]: %d\n", idx, balance.brace[idx]);
  }

  fprintf(stderr, "balance.quote: %d\n\n", balance.quote);
}
void checks(char *string)
{
  printf("string: `%s`\n\n", string);
  rl_line_buffer = string;
  int a = quote_in_string(string);
  int b = token_in_string(string, '(');
  int c = token_in_string(string, '[');
  fprintf(stderr, "quote_in_string: %d\n", a);
  fprintf(stderr, "brace_in_string: %d\n", c);
  fprintf(stderr, "paren_in_string: %d\n", b);
  print_balance();
  int d = match_balance('(', ')');
  int e = match_balance('[', ']');
  /*fprintf(stderr, "match_balance_paren: %d\n", d);*/
  /*fprintf(stderr, "match_balance_brace: %d\n", e);*/
}


int main()
{
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
  return 0;
}
