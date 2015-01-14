#define RL_EGG_TESTING 1
#define DEBUG 1
#define INLINE
#include "../trunk/readline-egg.c"

struct balnce {
  int paren;
  int quote;
  int brace;
} bal;

INLINE void
str_balncd_paren(char *str, int *paren_open_count)
{
  char *new_str = str_nquotd(str);
  struct delim_count *count;
  int close, open;

  if (*paren_open_count < 1) {
    count = strnof_delim(new_str, '(', ')', NULL);
    open = count->open;
    close = count->close;
    free(count);
  } else {
    struct delim_count tmp;
    tmp.open = *paren_open_count;
    count = strnof_delim(new_str, '(', ')', &tmp);
    open = count->open;
    close = count->close;
  }

  RL_EGG_DEBUG("%d\n", open);
  *paren_open_count = abs(open - close);
}

int main(void) {
  char *str = "(hello";
  char *_str = ":";
  struct delim_count *count = strnof_delim(str, '(', ')', NULL);
  printf("open: %d\nclose: %d\n", count->open, count->close);
  int diff = abs(count->open - count->close);
  printf("diff: %d\n", diff);
  str_balncd_paren(_str, &diff);
  printf("diff: %d\n", diff);
  struct delim_count *c = strnof_delim(_str, '(', ')', count);
  printf("open: %d\nclose: %d\n", c->open, c->close);
  free(count);
  return 0;
}
