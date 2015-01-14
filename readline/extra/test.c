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
  int idx[3];
  int *hdx;
  hdx = strnof_delim(str, '(', ')', idx);
  printf("%d\n", hdx[0]);
  return 0;
}
