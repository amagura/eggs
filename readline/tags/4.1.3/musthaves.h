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
#if !defined(RDLN_MUSTHAVES_H_GUARD)
# define RDLN_MUSTHAVES_H_GUARD 1
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

# if !defined(_GNU_SOURCE)
#  define _GNU_SOURCE 1
# endif

# if !defined(RD_DEBUG)
#  define RD_DEBUG 0
# endif

# if !defined(RD_TEST)
#  define RD_TEST 0
# endif

# if RD_DEBUG
#  define RD_DBG(format, ...)					\
     do {							\
	  fprintf(stderr, "## (%s)(%s)%d\n",			\
		  "RD_DBG:", __FILE__, __LINE__);		\
	  fprintf(stderr, "#  `%s'\n", __FUNCTION__);		\
	  fprintf(stderr, (format), ##__VA_ARGS__);		\
	  fprintf(stderr, "\n");	       			\
     } while(0)
# else
#  define RD_DBG(format, ...)
# endif

# if !defined(bzero)
#  define bzero(COM_B, COM_LEN)						\
     (memset((void *)(COM_B), '\0', (size_t)(COM_LEN)), (void)0)
# endif

# if !defined(bcopy)
#  define bcopy(COM_B1, COM_B2, COM_LEN)			\
     (memmove((void *)(COM_B2),					\
	      (const void *)(COM_B1),				\
	      (size_t)(COM_LEN)),				\
      (void)0)
# endif

# if !defined(mempcpy)
#  define mempcpy(COM_D, COM_S, COM_L)		\
     (memcpy((void *)(COM_D),			\
	     (const void *)(COM_S),		\
	     (size_t)(COM_L))			\
      + (size_t)(COM_L))
# endif

# if !defined(mempmove)
#  define mempmove(COM_D, COM_S, COM_L)		\
     (memmove((void *)(COM_D),			\
	      (const void *)(COM_S),		\
	      (size_t)(COM_L))			\
      + (size_t)(COM_L))
# endif

END_C_DECLS

#endif
