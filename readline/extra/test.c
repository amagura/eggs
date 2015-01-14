#define RL_EGG_TESTING 1
#define DEBUG 1
#define INLINE
#include "../trunk/readline-egg.c"

int main(void) {
  char *str = "{{((\"\"hello})\"";
  char *_str = ")}\"";
  int *idx = strnof_delim(str, '{', '}', NULL);
  int *hdx;
  printf("idx open: %d\nclose: %d\n", idx[0], idx[1]);
  hdx = strnof_delim(_str, '{', '}', idx);
  printf("hdx open: %d\nclose: %d\n", hdx[0], hdx[1]);
  return 0;
}
