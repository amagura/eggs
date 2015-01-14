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

struct count_t {
  int open;
  int close;
};

#if 0
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
#endif

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

struct delim_count_t {

  int open;
  int close;
};

  inline struct delim_count_t
strnof_delim_(char *str, char delim[2])
{



#define RL_EGG_STRNOF_DELIM(STRING_PTR, DELIM_PTR, DELIM_STRUCT)	\
  do {								        \
    if (!(DELIM_STRUCT))						\
      strnof_delim_(STRING_PTR, DELIM_PTR);				\
    else								\
      strnof_delim(STRING_PTR, DELIM_PTR, DELIM_STRUCT);		\
  } while(0)




  inline struct delim_cont_t * // returns -1 when stringp points to a string containing only NULL
    count_delim(char *stringp, char *delim, struct delim_count_t *count)
    {
      if (!(count == NULL))
        struct delim_count_t *_count;
      memset(count, 0, sizeof(*count));
      char *cp = &stringp[strlen(stringp)];

      if (cp == stringp)
        return NULL;

      do {
        if (cp != stringp && peek_chr(cp, stringp, false) == '\\')
          continue;

        if (*cp == close_delim)
          ++count->close;
        else if (*cp == open_delim)
          ++count->open;
      } while(cp-- != stringp);

      if (open_delim == close_delim && count->close > 0) {
        if (count->close % 2 == 1)
          while(count->open++ < --count->close);
        else {
          count->close %= 2;
        }
      }
      return count;
    }

  char *
    matching_delim_pos(char *stringp, char open_key, char close_key)
    {
      char *cp = &stringp[strlen(stringp)];

      do {

      } while(0);
    }

  inline int
    _skip(int pos, char *string, char open_delim, char close_delim)
    {
      while (--pos > -1) {
        if (pos > 0 && string[pos - 1] == '\\')
          continue;
        if (string[pos] == open_delim)
          return pos;
        else if (string[pos] == close_delim)
          pos = _skip(pos, string, open_delim, close_delim);
        else if (string[pos] == '"')
          pos = _skip(pos, string, '"', '"');
      }
      return pos;
    }

  int main()
  {
    char string[BUFSIZ] = "{{{{{helloa}";
    char *cp = &string[strlen(string)];
    char strin[BUFSIZ] = "}}}}";
    struct delim_count_t *count;
    count_delim(string, '{', '}', NULL);
    printf("open: %d\nclose: %d\n", count->open, count->close);
    printf("match pos: %d\n", _skip(strlen(string)-1, string, '{', '}'));
    char *_cp = strrchr(string, '{');
    printf("same %d\n", _cp == &string[4]);
    printf("_cp: %c\n", *_cp);


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
