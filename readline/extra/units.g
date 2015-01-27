#define RL_EGG_TESTING 1
#define DEBUG 1
#define INLINE
#include "../trunk/readline-egg.c"

#ifndef RL_EGG_TEST_C
struct unbalanced {
  char *paren;
  char *brace;
  char *dquote;
} unbal;

@define START_TEST #1 #2 \
{			 \
RL_EGG_BEGIN_TRACE
@define END_TEST         \
RL_EGG_END_TRACE;	 \
@ifdef #1		 \
return #1		 \
@endif			 \
}
START_TEST(void prompt_balance);

END_TEST();

#define START_TEST(TEST_NAME) TEST_NAME {	\
  RL_EGG_BEGIN_TRACE
#define END_TEST RL_EGG_END_TRACE }
START_TEST(int prompt_balance);
END_TEST;

int main()
{
  RL_EGG_DEBUG("check_prompt_balance: %d\n", RL_EGG_BOOLSTR(check_prompt_balance("(hello")));
  RL_EGG_DEBUG("check_prompt_balance: %d\n", RL_EGG_BOOLSTR(check_prompt_balance(")")));
  return 0;
}
#endif
