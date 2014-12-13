#include <stdio.h>
#include <stdlib.h>
#include <readline/history.h>
#include <readline/readline.h>

#ifdef HAVE_LIBBSD_STRING_H
#include <bsd/string.h>
#endif

int main(void)
{
  using_history();
  add_history("a");
  add_history("cdfg");
  HIST_ENTRY **hist_list = history_list();
  char result_buf[BUFSIZ];
  int idx;

  if (hist_list == NULL)
    return EXIT_FAILURE;

#if HAVE_LIBBSD_STRING_H
  strlcpy(result_buf, "0 ", sizeof result_buf);
  strlcat(result_buf, hist_list[0]->line, sizeof result_buf);
  strlcat(result_buf, "\n", sizeof result_buf);
#else
#define __TEST_RESULT_BUF_SIZE sizeof result_buf - strlen(result_buf) - 1
  strncat(result_buf, "0 ", __TEST_RESULT_BUF_SIZE);
  strncat(result_buf, hist_list[0]->line, __TEST_RESULT_BUF_SIZE);
  strncat(result_buf, "\n", __TEST_RESULT_BUF_SIZE);
#endif

  char tmp_conv_standin[history_length-1];
  for (idx = 1; idx < history_length; ++idx) {
    sprintf(tmp_conv_standin, "%d", idx);
#ifdef HAVE_LIBBSD_STRING_H
    strlcat(result_buf, tmp_conv_standin, sizeof result_buf);
    strlcat(result_buf, " ", sizeof result_buf);
    strlcat(result_buf, hist_list[idx]->line, sizeof result_buf);
    strlcat(result_buf, "\n", sizeof result_buf);
#else
    strncat(result_buf, tmp_conv_standin, (sizeof result_buf - BUFSIZ - 1));
    strncat(result_buf, " ", (sizeof result_buf - BUFSIZ - 1));
    strncat(result_buf, hist_list[idx]->line, (sizeof result_buf - BUFSIZ - 1));
    strncat(result_buf, "\n", (sizeof result_buf - BUFSIZ - 1));
#endif
    //snprintf(result_buf, sizeof result_buf, "%d %s", (idx - 1), hist_list[idx]->line);
  }
  printf("%s\n", result_buf);

  return EXIT_SUCCESS;
}
