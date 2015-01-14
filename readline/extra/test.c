#define RL_EGG_TESTING 1
#define DEBUG 1
#define INLINE
#include "../trunk/readline-egg.c"

struct balnce {
  int paren;
  int quote;
  int brace;
} bal;

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
