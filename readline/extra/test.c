#define _GNU_SOURCE 1
#define RL_EGG_TESTING 1
#define DEBUG 1
#define INLINE
#include <stdarg.h>
#include "../trunk/readline-egg.c"

struct balnce {
  int paren;
  int quote;
  int brace;
} bal;

int main(void) {
  char *str = "blue";
  char *nstr = memsafe_concat("hello, ", str, NULL);
  printf("%s\n", nstr);
  free(nstr);
  char *tmp = str_unquotd("\"blue\"a");
  printf("%s\n", tmp);
  free(tmp);
  return 0;
}
