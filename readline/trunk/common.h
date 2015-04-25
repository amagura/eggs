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
#ifndef COMMON_MAIN_H_GUARD
# define COMMON_MAIN_H_GUARD 1
/* BEGIN_C_DECLS should be used at the beginning of your declarations,
so that C++ compilers don't mangle their names.  Use END_C_DECLS at
the end of C declarations. */
#undef BEGIN_C_DECLS
#undef END_C_DECLS
#ifdef __cplusplus
# define BEGIN_C_DECLS extern "C" {
# define END_C_DECLS }
#else
# define BEGIN_C_DECLS /* empty */
# define END_C_DECLS /* empty */
#endif

/* PARAMS is a macro used to wrap function prototypes, so that
  compilers that don't understand ANSI C prototypes still work,
  and ANSI C compilers can issue warnings about type mismatches. */
#undef PARAMS
#if defined (__STDC__) || defined (_AIX) \
       || (defined (__mips) && defined (_SYSTYPE_SVR4)) \
       || defined(WIN32) || defined(__cplusplus)
# define PARAMS(protos) protos
#else
# define PARAMS(protos) ()
#endif

BEGIN_C_DECLS

#include <stdio.h>

# if 1
void *memset PARAMS((void *s, int c, size_t n));
void *memcpy PARAMS((void *dest, const void *src, size_t n));
# else
#  include <string.h>
# endif

# ifndef COM_DEBUG
#  define COM_DEBUG 1 // XXX change this to turn debug messages on/off
# endif

# if COM_DEBUG
#  ifndef COM_DLVL
#   define COM_DLVL 2 // XXX change this to increase/decrease debug verbosity
#  endif
# endif

# ifndef COM_MACROS
#  define COM_MACROS 0 // XXX change this to use or not use macros vs inline
# endif

# ifndef PACKAGE_VERSION
#  define PACKAGE_VERSION ""
# endif

# ifndef COM_PROGNAME
#  define COM_PROGNAME "common"
# endif

# if COM_DEBUG
#  if defined(__linux__) || defined(__gnu_linux__)
#   include <mcheck.h>
#  endif
#  define COM_DBG(format, ...)					\
     do {							\
	  fprintf(stderr, "## (%s)(%s)%d\n",			\
		  COM_PROGNAME, __FILE__, __LINE__);		\
	  fprintf(stderr, "#  `%s'\n", __FUNCTION__);		\
	  fprintf(stderr, (format), ##__VA_ARGS__);		\
	  fprintf(stderr, "\n");	       			\
     } while(0)
#  define COM_SDBG(format, exp)				\
     do {						\
	  fprintf(stderr, "## (%s)(%s)%d\n",		\
		  COM_PROGNAME, __FILE__, __LINE__);	\
	  fprintf(stderr, "#  `%s`\n", __FUNCTION__);	\
	  fprintf(stderr, (format), (exp));		\
	  fprintf(stderr, "\n");			\
     } while(0)
#  define COM_ONDBG(...) (__VA_ARGS__)
#  define COM_XONDBG(COM_X) COM_X
#  define com_ping COM_DBG("\n^^^^ %s ^^^^\n", "MARCO!")
#  define com_pong COM_DBG("\n$$$$ %s $$$$\n", "POLO!")
# else
#  define COM_DBG(format, ...)
#  define COM_SDBG(format, exp)
#  define COM_ONDBG(...)
#  define COM_XONDBG(COM_X)
#  define com_ping
#  define com_pong
# endif

# define COM_ERROR(format, ...)				\
     do {						\
	  fprintf(stderr, "%s:err: ", COM_PROGNAME);	\
	  fprintf(stderr, (format), __VA_ARGS__);	\
	  fprintf(stderr,				\
		  "\nin %s:{%d}:%s()\n",		\
		  __FILE__,				\
		  __LINE__,				\
		  __FUNCTION__);			\
     } while(0)

# define com_usage(format) (printf((format), (COM_PROGNAME)));

# define com_arg(opt, desc, tabs) (printf("  %s%s%s\n", (opt),(tabs),(desc)));

# define com_arg_eol_tabs "\n\t\t\t\t"
# define com_help(usage, tabs)				\
     do {						\
	  com_usage((usage));				\
	  com_arg("-h, --help",				\
		  "print this message and exit",	\
		  (tabs));				\
	  com_arg("-v, --version",			\
		  "print program version and exit",	\
		  (tabs));				\
     } while(0)

# if HAVE_LIBBSD
#  include <limits.h>
#  include <bsd/stdlib.h>
// FIXME, the following macros shouldn't call `exit' or `perror'.
#  define COM_STRTONUM(dst_num, const_string)			\
     do {							\
	  errno = 0;						\
	  ((dst_num) = strtonum((const_string),			\
				INT_MIN,			\
				INT_MAX,			\
				NULL));				\
	  if (errno != 0) {					\
	       perror(COM_PROGNAME);				\
	       exit(EXIT_FAILURE);				\
	  }							\
     } while(0)
# else
#  define COM_STRTONUM(dst_num, const_string)			\
     do {							\
	  errno = 0;						\
	  ((dst_num) = strtol((const_string), NULL, 10));	\
	  if (errno != 0) {					\
	       perror(COM_PROGNAME);				\
	       exit(EXIT_FAILURE);				\
	  }							\
     } while(0)
# endif

# ifndef bzero
#  if COM_MACROS
#   define bzero(COM_B, COM_LEN)			\
     (memset((COM_B), '\0', (COM_LEN)), (void) 0)
#  else
inline void bzero(void *b, size_t len)
{
     return (memset(b, '\0', len), (void) 0);
}
#  endif
# endif

# ifndef mempcpy
#  if COM_MACROS
#   define mempcpy(COM_D, COM_S, COM_L)			\
     (memcpy((COM_D), (COM_S), (COM_L) + (COM_L)))
#  else
inline void *mempcpy(void *dst, void *src, size_t len)
{
     return (memcpy(dst, src, len) + len);
}
#  endif
# endif

/** Function Prototypes **/

/** cpeek: access the character either before or after the current character
 ** in an array of characters.  **/
char cpeek PARAMS((char *c, char *s, short fwd));

/** intlen: find how many digits a given integral contains. **/
int intlen PARAMS((int n));
int intlenc PARAMS((const int n));
size_t intlenm PARAMS((int src)); /* XXX for use with malloc'ing for
				   * calls to `itoa' */

/** rev: reverse an array of characters **/
void rev PARAMS((char *s));
char *revp PARAMS((const char *s)); /* XXX return value needs free */

/** itoa: convert a number to an atom (i.e. string) **/
void itoa PARAMS((char *dst, int src));
char *itoap PARAMS((int src));

/** concat: catenate several strings together
 ** XXX return value needs free.  **/
char *concat PARAMS((const char *s1, ...));
# undef cat
# define cat(...) (concat(__VA_ARGS__, (void *)NULL))

/** concatl: catenate as many _s_ource strings into `buf'
 ** as will fit in `bufsiz' bytes **/
/** XXX if you _must_ use concatl directly: be sure to pass `(void *)NULL' as
 ** the last argument **/
size_t concatl PARAMS((char *buf, size_t bufsiz, const char *s1, ...));
# undef catl
# define catl(...) (concatl(__VA_ARGS__, (void *)NULL))

/** repeat: create an array of chars containing n-1 many _s_ chars **/
void repeat PARAMS((char *dst, const char s, size_t n));
int strrep PARAMS((char *dst, const char *s, size_t n));
char *strprep PARAMS((const char *s, int x));

END_C_DECLS

#endif /* COMMONS_MAIN_H_GUARD */
