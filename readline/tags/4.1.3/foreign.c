/****
Copyright 2015 Alexej Magura

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
****/

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <limits.h>
#include <stdarg.h>

#include "foreign.h"
#include "musthaves.h"

char *concat(const char *s1, ...)
{
	va_list args;
	const char *s;
	char *p, *result;
	unsigned long l, m, n;

	m = n = strlen(s1);
	va_start(args, s1);
	while ((s = va_arg(args, char *))) {
		l = strlen(s);
		if ((m += l) < l) break;
	}
	va_end(args);
	if (s || m >= INT_MAX) return NULL;

#if defined(__cplusplus)
	result = (char *)malloc(m + 1);
#else
	result = malloc(m + 1);
#endif
	if (!result) return NULL;

	memcpy(p = result, s1, n);
	p += n;
	va_start(args, s1);
	while ((s = va_arg(args, char *))) {
		l = strlen(s);
		if ((n += l) < l || n > m) break;
		memcpy(p, s, l);
		p += l;
	}
	va_end(args);
	if (s || m != n || p != result + n) {
		free(result);
		return NULL;
	}

	*p = '\0';
	return result;
}

/* unlike `concat', which returns a
 * new pointer that must then be copied
 * or acted upon in some meaningfully meaningless
 * manner, `catl' returns the number of bytes belonging
 * to `buf', which could _NOT_ be filled, always copying
 * no more than `bufsiz` of data into `buf'
 *
 * If the return value is an integral value, which
 * we'll call `y', that is less than 0,
 * then the resulting catenation has been truncated by `!y'
 * many bytes.  Similarlly, if a positive value is returned:
 * `y' many bytes is how much of `buf', which was _NOT_ used.
 *
 * XXX A failure is indicated by a return value _equal to
 * the destination buffers size_, which may make errors somewhat
 * harder to spot! */
size_t concatl(char *dst, size_t sz, const char *s1, ...)
{
     va_list args;
     const char *s = NULL;
     char *p, *tmp;
     unsigned long ldx, mdx, ndx;
     size_t used = 0;

     mdx = ndx = strlen(s1);
     va_start(args, s1);
     while ((s = va_arg(args, char *))) {
	  ldx = strlen(s);
	  if ((mdx += ldx) < ldx) break;
     }
     va_end(args);
     if (s || mdx >= INT_MAX) return sz;

#if defined(__cplusplus)
     tmp = (char *)malloc(mdx + 1);
#else
     tmp = malloc(mdx + 1);
#endif
     if (!tmp) return sz;
     bzero(tmp, mdx + 1);
     bzero(dst, mdx + 1);

     p = tmp;
     p = mempcpy(p, (char *)s1, ndx);

     used += ndx;
     RD_DBG("p: `%s`\n", p);
     RD_DBG("used: %lu\n", used - 0);

     va_start(args, s1);
     while ((s = va_arg(args, char *))) {
	  ldx = strlen(s);
	  if ((ndx += ldx) < ldx || ndx > mdx) break;
	  p = mempcpy(p, (char *)s, ldx);
	  used += ldx;
     }
     va_end(args);
     if (s || mdx != ndx || p != tmp + ndx) {
	  free(tmp);
	  return sz;
     }

     RD_DBG("tmp: `%s'\n", tmp);
     p = mempcpy(dst, tmp, (used > sz ? sz : used));
     free(tmp);
     *p = '\0';
     ++used;

     RD_DBG("dst: `%s'\n", dst);
     RD_DBG("*p: `%c'\n", *p);
     RD_DBG("*--p: `%c'\n", cpeek(p, dst, 0));
     RD_DBG("strlen(dst): %lu\n", strlen(dst));
     RD_DBG("used#2: %lu\n", used - 0);

     return (used > sz ? 0 : sz - used);
}

/* concatm is a little different:
 * unlike `concatl' or `concat', concatm _moves_ memory: that is, the destination
 * pointer can be passed as an argument. */
size_t concatm(char *dst, size_t sz, const char *s1, ...)
{
     va_list args;
     const char *s = NULL;
     char *p, *tmp;
     unsigned long ldx, mdx, ndx;
     size_t used = 0;

     mdx = ndx = strlen(s1);
     va_start(args, s1);
     while ((s = va_arg(args, char *))) {
	  ldx = strlen(s);
	  if ((mdx += ldx) < ldx) break;
     }
     va_end(args);
     if (s || mdx >= INT_MAX) return sz;

#if defined(__cplusplus)
     tmp = (char *)malloc(mdx + 1);
#else
     tmp = malloc(mdx + 1);
#endif
     if (!tmp) return sz;
     bzero(tmp, mdx + 1);

     p = tmp;
     p = mempcpy(p, (char *)s1, ndx);

     used += ndx;
     RD_DBG("p: `%s`\n", p);
     RD_DBG("used: %lu\n", used - 0);

     va_start(args, s1);
     while ((s = va_arg(args, char *))) {
	  ldx = strlen(s);
	  if ((ndx += ldx) < ldx || ndx > mdx) break;
	  p = mempcpy(p, (char *)s, ldx);
	  used += ldx;
     }
     va_end(args);
     if (s || mdx != ndx || p != tmp + ndx) {
	  free(tmp);
	  return sz;
     }
     RD_DBG("tmp: `%s'\n", tmp);
#if defined(mempmove) && 0
     p = mempmove(dst, tmp, (used > sz ? sz : used));
#else
     memmove(dst, tmp, (used > sz ? sz : used));
     p = &dst[(used > sz ? sz : used)];
#endif
     free(tmp);
     *p = '\0';
     ++used;

     RD_DBG("dst: `%s'\n", dst);
     RD_DBG("*p: `%c'\n", *p);
     RD_DBG("*--p: `%c'\n", cpeek(p, dst, 0));
     RD_DBG("strlen(dst): %lu\n", strlen(dst));
     RD_DBG("used#2: %lu\n", used - 0);

     return (used > sz ? 0 : sz - used);
}

char cpeek(const char *c, const char *s, const short fwd)
{
     if (fwd > 0) {
	  if (*c == '\0'
# if defined(_GNU_SOURCE)
	      || c == strchr(s, '\0') - 1
# else
	      || c == &s[strlen(s)]
# endif
	       )
	       return *c;
	  else
	       return *(c + 1);
     }
     return (c == s) ? *c : *(c - 1);
}

# if 0
static int strnof_delim(char *str, char odelim, char cdelim, int count[2])
{
  memset(count, 0, sizeof(*count)*2);
  RL_EGG_BEGIN_TRACE;
  RL_EGG_DEBUG("str: %s\n", str);
  char *cp = strchr(str, '\0');
  RL_EGG_END_TRACE;

  if (cp == str)
    return 1;

  do {
    if (cp != str && cpeek(cp, str, false) == '\\')
      continue;

    if (*cp == cdelim) {
      ++count[1]; // increment close
    } else if (*cp == odelim) {
      ++count[0]; // increment open
    }
  } while(cp-- != str);

  if (odelim == cdelim && count[1] > 0) {
    if (count[1] % 2 == 1)
      while(count[0]++ < --count[1]);
    else {
      count[0] = count[1] * 0.5;
      count[1] *= 0.5;
    }
  }

  RL_EGG_BEGIN_TRACE;
  RL_EGG_DEBUG("open: %d\nclose: %d\n", count[0], count[1]);
  RL_EGG_END_TRACE;
  return 0;
}
# endif

int *strndelim(const char *s, const char od, const char cd, int count[2])
{
     memset(count, 0, sizeof(*count)*2);
     char *c = strchr(s, '\0');

     if (c == s)
	  return NULL;

     do {
	  if (c != s && cpeek(c, s, 0) == '\\')
	       continue;
	  if (*c == cd)
	       ++count[1];
	  else if (*c == od)
	       ++count[0];
     } while (c-- != s);

     if (od == cd && count[1] > 0) {
	  if (count[1] % 2 == 1)
	       while (count[0]++ < --count[1]);
	  else {
	       count[0] = count[1] * 0.5;
	       count[1] *= 0.5;
	  }
     }

     return count;
}

char *strwodqp(const char *src)
{
     size_t n = strlen(src) + 1;
     int c[2] = {0, 0}, even = 0;
     char *tmp, *token, *rest, *newp;
     tmp = token = rest = newp = NULL;

     if (!strndelim(src, '"', '"', c))
	  return NULL;

     if (c[0] == 0)
	  return NULL;

     tmp = strdup(src);
     newp = malloc(n);
     even = c[0] - abs(c[0] - c[1]);

     token = strtok_r(tmp, "\"", &rest);

     if (token == NULL) {
	  free(newp);
	  return NULL;
     }

     catl(newp, n, token);
     while ((token = strtok_r(NULL, "\"", &rest)) != NULL) {
	  if (even % 2 == 1) {
	       catm(newp, n, newp, token);
	       --even;
	  } else {
	       ++even;
	  }
     }

     free(tmp);
     return newp;
}
