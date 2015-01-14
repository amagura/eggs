#define RL_EGG_TESTING 1
#define DEBUG 1
#define INLINE
#include "../trunk/readline-egg.c"

char *
str_nquotd(char *str)
{
  int *idx = strnof_delim(str, '"', '"', NULL);
  RL_EGG_DEBUG("idx[0]: %d\n", idx[0]);
  RL_EGG_DEBUG("idx[1]: %d\n", idx[1]);

  if (idx[0] == 0) {
    free(idx);
    return str;
  }
  int even = idx[0] - abs(idx[0] - idx[1]);
  free(idx);

  char *token, *rest, *tmp, *result;
  tmp = NULL;
  char str_slice[BUFSIZ]; // FIXME would like to use malloc, but can't seem to allocate enough memory to fit everything...

  result = str_slice;
  tmp = strdup(str);

  if (tmp == NULL) {
    perror(__FUNCTION__);
    return str;
  }

  token = strtok_r(tmp, "\"", &rest);
  RL_EGG_DEBUG("token: %s\n", token);
  RL_EGG_STRCAT(str_slice, token);

  bool need_free = true;

  while ((token = strtok_r(NULL, "\"", &rest)) != NULL) {
    RL_EGG_DEBUG("token (while): %s\n", token);
    RL_EGG_DEBUG("even (while): %d\n", even);
    if (even % 2 == 1) {
      RL_EGG_STRCAT(str_slice, token);
      --even;
    } else {
      ++even;
    }
    if (need_free) {
      free(tmp);
      need_free = false;
    }
    tmp = rest;
  }
  return result;
}


int main(void) {
  char *str = "{\"hello})\"blue\"red\"orange";
  char *_str = "}";
  char *__str = str_nquotd(str);
  printf("not quoted %s\n", __str);
  return 0;
}
