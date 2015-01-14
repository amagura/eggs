#define RL_EGG_TESTING 1
#define DEBUG 1
#define INLINE
#include "../trunk/readline-egg.c"

int main(void) {
  char *str = "{\"hello})\"blue\"red\"orange";
  char *_str = "}";
  char *__str = str_nquotd(str);
  printf("not quoted %s\n", __str);
  return 0;
}
