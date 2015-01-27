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
  check_prompt_balance("(hello");
  printf("%d\n", track_balnc('(', 0));
  check_prompt_balance(")");
  printf("%d\n", track_balnc('(', 0));
  return 0;
}
