/* -*-c-*- */
#define RL_EGG_TESTING 1
#define DEBUG 1
#define INLINE
#include "../trunk/readline-egg.c"

#ifndef RL_EGG_TEST_C
#include <CUnit/CUnit.h>
#include <CUnit/Automated.h>
struct unbalanced {
  char *paren;
  char *brace;
  char *dquote;
} unbal;

void test_chkbal(void)
{
  @START_TRACE;
  CU_ASSERT(check_prompt_balance("[hello") == 1);
  CU_ASSERT(check_prompt_balance("[hello]]") == 1);
  @END_TRACE;
}

int main()
{
  CU_initialize_registry();
  CU_pSuite prompt = CU_add_suite("prompt balance", NULL, NULL);
  CU_add_test(prompt, "test_chkbal", test_chkbal);
  CU_automated_run_tests();
  return 0;
}
#endif
