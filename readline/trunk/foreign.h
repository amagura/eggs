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
#if !defined(RDLN_FOREIGN_H_GUARD)
# define RDLN_FOREIGN_H_GUARD 1
/* BEGIN_C_DECLS should be used at the beginning of your declarations,
so that C++ compilers don't mangle their names.  Use END_C_DECLS at
the end of C declarations. */
# undef BEGIN_C_DECLS
# undef END_C_DECLS
# ifdef __cplusplus
#  define BEGIN_C_DECLS extern "C" {
#  define END_C_DECLS }
# else
#  define BEGIN_C_DECLS /* empty */
#  define END_C_DECLS /* empty */
# endif

/* PARAMS is a macro used to wrap function prototypes, so that
  compilers that don't understand ANSI C prototypes still work,
  and ANSI C compilers can issue warnings about type mismatches. */
# undef PARAMS
# if defined (__STDC__) || defined (_AIX) \
     || (defined (__mips) && defined (_SYSTYPE_SVR4))	\
     || defined(WIN32) || defined(__cplusplus)
#  define PARAMS(protos) protos
# else
#  define PARAMS(protos) ()
# endif

BEGIN_C_DECLS

# include <stdio.h>

char *concat PARAMS((const char *s1, ...));
size_t concatl PARAMS((char *dst, size_t sz, const char *s1, ...));
size_t concatm PARAMS((char *dst, size_t sz, const char *s1, ...));

char cpeek PARAMS((const char *c, const char *s, const short fwd));

int *strndelim PARAMS((const char *s, const char od, const char cd, int count[2]));

char *strwodqp PARAMS((const char *src));

# define cat(...) (concat(__VA_ARGS__, (void *)NULL))
# define catl(...) (concatl(__VA_ARGS__, (void *)NULL))
# define catm(...) (concatm(__VA_ARGS__, (void *)NULL))

END_C_DECLS

#endif
