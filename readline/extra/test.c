#define RL_EGG_TESTING 1
#define DEBUG 1
#define INLINE
#include "../trunk/readline-egg.c"

int main(void) {
  char *str = "{{hello}";
  char *_str = ")";
  int *idx;
  idx = strnof_delim(str, '{', '}', NULL);
  printf("open: %d\nclose: %d\n", idx[0], idx[1]);
  return 0;
}
