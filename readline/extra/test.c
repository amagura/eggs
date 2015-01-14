#define RL_EGG_TESTING 1
#include "../trunk/readline-egg.c"

int main(void) {
  char *str = "((hello)";
  char *_str = ")";
  struct delim_count count;
  count = strnof_delim_nstrct(str, '(', ')');
  printf("open: %d\nclose: %d\n", count.open);
}
