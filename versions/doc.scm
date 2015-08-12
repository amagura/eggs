;;;; egg:       versions
;;;; file:      doc.scm
;;;; author:    elf <elf@ephemeral.net>
;;;; date:      07 Apr 2008
;;;; licence:   BSD (see LICENCE)
;;;; dialect:   r5rs
;;;; requires:  chicken build tools, eggdoc
;;;; version:   1.0
;;;; purpose:   eggdoc-formatted documentation for versions
;;;;
;;;; history:   1.1  20080410 (Ivan Raikov)  Corrected location of LICENCE file
;;;; history:   1.0  20080407 (elf)  Initial release
;;;;




(use srfi-1)    ; list library
(use utils)     ; utility procedures
(use eggdoc)    ; egg documentation facility



;; (table-make FORMAT-WRAPPERS HEADERS ROWS...)
;; helper macro for constructing tables
(define (elf-eggdoc-ss doc)
    `((table-spec *macro* .
        ,(lambda (tag fmtw hdrs . rows)
            `(table (@ (style "table-layout: auto"))
                 (tr (@ (style "vertical-align: baseline"))
                     ,@(map
                         (lambda (h)
                             `(th (@ (style "padding-top: 2.5em")) ,h))
                         hdrs))
                     ,@(pair-fold-right
                     (lambda (x r)
                         (let ((t   (if (null? (cdr x))
                                        "padding-top: 2em; padding-bottom: 2.5em"
                                        "padding-top: 2em"))
                               (a   (car x)))
                             (cons
                                 `(tr (@ (style "vertical-align: baseline"))
                                      ,@(map
                                          (lambda (f d)
                                              `(td (@ (style ,t)) (,f ,d)))
                                          fmtw a))
                                 r)))
                     '()
                     rows))))
      (inline-table-spec *macro* .
        ,(lambda (tag fmtw hdrs . rows)
            `(table (@ (style "inline-table; table-layout: auto"))
                 (tr (@ (style "vertical-align: baseline"))
                     ,@(map
                         (lambda (h)
                             `(th (@ (style "padding-top: 1.5em")) ,h))
                         hdrs))
                     ,@(pair-fold-right
                     (lambda (x r)
                         (let ((t   (if (null? (cdr x))
                                        "padding-top: 1em; padding-bottom: 1.5em"
                                        "padding-top: 1em"))
                               (a   (car x)))
                             (cons
                                 `(tr (@ (style "vertical-align: baseline"))
                                      ,@(map
                                          (lambda (f d)
                                              `(td (@ (style ,t)) (,f ,d)))
                                          fmtw a))
                                 r)))
                     '()
                     rows))))
        ,@(eggdoc:make-stylesheet doc)))


(define doc
    `((eggdoc:begin
        (name         "versions")
        (description  "versioning procedures")
        (author       (url "mailto:elf@ephemeral.net" "elf"))

        (history
            (version "1.2" "20080709 (elf)  Fix for version->string")
            (version "1.1" "20080410 (Ivan Raikov)  Minor fixes to egg-related files.")
            (version "1.0" "20080407 (elf)  Initial release."))

        (usage)
        (download "versions.egg")


        (documentation

            (section "Introduction"
                (p "This extension provides procedures for handling versioning "
                   "of arbitrary packages.  It should simplify determining "
                   "when new packages are available based on some string "
                   "containing version data.  This should, hopefully, make "
                   "life easier for maintainers."))
            
            (section "Version Strings"
                (p "Version strings have the following format:")
                (p (tt "[LABEL][MAJOR][MINOR][MICRO][PATCH][EXTRA]"))
                (p (tt "MAJOR") ", " (tt "MINOR") ", " (tt "MICRO") " (if "
                   "given), and all segments of " (tt "PATCH") " (if given) "
                   "must be separated by dots (" (tt ".") ").  The version "
                   "strings have the following semantics:")
                (table-spec
                    (tt div tt div)
                    ("Segment" "Required?" "Format" "Description")
                    ("LABEL" "no" "[any chars]" 
                     ("Any characters, including digits, are allowed.  If "
                      "the " (tt "LABEL") " ends with a digit character, "
                      "some separator character BESIDES " (tt ".") " is "
                      "required; otherwise, the end of " (tt "LABEL") " will "
                      "be parsed as the " (tt "MAJOR") " value.  The "
                      (tt "LABEL") " segment usually corresponds to the "
                      "name of the package."))
                    ("MAJOR" "yes" "[integer]"
                     "Major release version.")
                    ("MINOR" "yes" ".[integer]"
                     "Minor release version.")
                    ("MICRO" "no" ".[integer]"
                     "Micro release version.")
                    ("PATCH" "no" "(.[integer])..."
                     ("Patch version.  This may be composed of any number of "
                      "segments; there is no limit.  (This is to allow parsing "
                      "of version control tree versions.)"))
                    ("EXTRA" "no" "[any chars]"
                     ("Extra version suffix, usually for local or maintainer "
                      "versions.  It must be separated from the final element "
                      "by a non-digit, non-dot character.")))
                (p "The following table gives some examples of valid version "
                   "strings along with their breakdowns:")
                (table-spec
                    (tt tt tt tt tt tt tt)
                    ("String" "LABEL" "MAJOR" "MINOR" "MICRO" "PATCH" "EXTRA")
                    ("1.2" "#f" "1" "2" "#f" "#f" "#f")
                    ("1.2.3" "#f" "1" "2" "3" "#f" "#f")
                    ("1.2.3.4" "#f" "1" "2" "3" "(4)" "#f")
                    ("1.2.3.4.5.6" "#f" "1" "2" "3" "(4 5 6)" "#f")
                    ("1.2-extra" "#f" "1" "2" "#f" "#f" "-extra")
                    ("1.2a3" "#f" "1" "2" "#f" "#f" "a3")
                    ("1.2.3-extra" "#f" "1" "2" "3" "#f" "-extra")
                    ("1.2.3a3" "#f" "1" "2" "3" "#f" "a3")
                    ("1.2.3-4extra" "#f" "1" "2" "3" "#f" "-4extra")
                    ("1.2.3.4.5-extra" "#f" "1" "2" "3" "(4 5)" "-extra")
                    ("foo-1.2.3" "foo-" "1" "2" "3" "#f" "#f")
                    ("foo_1.2.3" "foo_" "1" "2" "3" "#f" "#f")
                    ("foo1.2.3" "foo" "1" "2" "3" "#f" "#f")
                    ("foo1-2.3.4" "foo1-" "2" "3" "4" "#f" "#f"))
                (p "The following examples are NOT valid version strings:")
                (table-spec
                    (tt div)
                    ("String" "Reason")
                    ("foo-1-2-3"
                     ("Version number components must be separated by "
                      (tt ".") " (dot)."))
                    ("foo-1.a3"
                     ("Minor release number is required."))
                    ("1a3"
                     "Minor release is required.")))

            (section "Version Records"
                (group
                    (procedure "(make-version MAJOR MINOR #!key (label #f) (micro #f) (patch #f) (extra #f))"
                        (p "Creates a new version record with the given "
                           "components.  " (tt "MAJOR") ", " (tt "MINOR") 
                           ", and " (tt "MICRO") " (if given) must be exact "
                           "non-negative integers.  " (tt "LABEL") " and "
                           (tt "EXTRA") ", if given, must be non-null "
                           "strings.  " (tt "PATCH") " (if given) must be "
                           "either a non-negative exact integer or a "
                           "list/vector composed of non-negative exact "
                           "integers."))
                    (procedure "(version? OBJ)"
                        (p "Returns " (tt "#t") " if " (tt "OBJ") " is a "
                           "version record."))
                    (definition
                        (signatures
                            (signature "procedure" "(version:label VERSION)")
                            (signature "procedure" "(version:major VERSION)")
                            (signature "procedure" "(version:minor VERSION)")
                            (signature "procedure" "(version:micro VERSION)")
                            (signature "procedure" "(version:patch VERSION)")
                            (signature "procedure" "(version:extra VERSION)"))
                        (p "Returns the specified field of the " 
                           (tt "VERSION") " record."))))

            (section "Conversion Procedures"
                (group
                    (procedure "(string->version VERSION-STRING)"
                        (p "If " (tt "VERSION-STRING") " is a string with "
                           "valid version data, returns a version record "
                           "with its parsed components.  Otherwise, return "
                           (tt "#f") "."))
                    (procedure "(version->string VERSION)"
                        (p "Returns the string representation of the given "
                           (tt "VERSION") " record."))))

            (section "Version Comparison"
                (subsection "Comparison Rules"
                    (p "Two version objects, " (tt "v1") " and " (tt "v2") 
                       " are ordered via the rules below.  (The " 
                       (tt "version:") " prefix is ommitted for conciseness.)  "
                       "All string fields are compared with the "
                       (tt "string=?") ", " (tt "string<?") ", and " 
                       (tt "string>?") " procedures.  All numeric fields or "
                       "numeric field components are compared with the "
                       (tt "fx=") ", " (tt "fx<") ", and " (tt "fx>") 
                       " procedures.  The " (tt "patch") " field, below, is "
                       "first checked against the entire field for existence, "
                       "then each element (if it exists) is compared in order, "
                       "with an implicit last element of " (tt "#f") ".")
                    (table-spec
                        (tt tt tt tt div)
                        ("Field" "v1 Field Value" "v2 Field Value" "Comparison"
                         "Result")
                        ("label" "exists" "exists" "="
                         ("continue to " (tt "major")))
                        ("" "" "" "<" "v1 < v2")
                        ("" "" "" ">" "v1 > v2")
                        ("" "exists" "#f" "" "v1 > v2")
                        ("" "#f" "exists" "" "v1 < v2")
                        ("" "#f" "#f" "" ("continue to " (tt "major")))
                        ("major" "exists" "exists" "="
                         ("continue to " (tt "minor")))
                        ("" "" "" "<" "v1 < v2")
                        ("" "" "" ">" "v2 > v2")
                        ("minor" "exists" "exists" "="
                         ("continue to " (tt "micro")))
                        ("" "" "" "<" "v1 < v2")
                        ("" "" "" ">" "v1 > v2")
                        ("micro" "exists" "exists" "="
                         ("continue to " (tt "patch")))
                        ("" "" "" "<" "v1 < v2")
                        ("" "" "" ">" "v1 > v2")
                        ("" "exists" "#f" "" "v1 > v2")
                        ("" "#f" "exists" "" "v1 < v2")
                        ("" "#f" "#f" "" ("continue to " (tt "patch")))
                        ("patch" "exists" "exists" "=" "continue")
                        ("" "" "" "<" "v1 < v2")
                        ("" "" "" ">" "v1 > v2")
                        ("" "exists" "#f" "" "v1 > v2")
                        ("" "#f" "exists" "" "v1 < v2")
                        ("" "#f" "#f" "" ("continue to " (tt "extra")))
                        ("extra" "exists" "exists" "=" "v1 = v2")
                        ("" "" "" "<" "v1 < v2")
                        ("" "" "" ">" "v1 > v2")
                        ("" "exists" "#f" "" "v1 > v2")
                        ("" "#f" "exists" "" "v1 < v2")
                        ("" "#f" "#f" "" "v1 = v2"))
                    (p "Some examples:")
                    (p (pre "1.0 < foo-1.0"))
                    (p (pre "1.0 < 1.0.0"))
                    (p (pre "1.0-squid < 1.0.0"))
                    (p (pre "1.0.0 < 1.0.0-squid"))
                    (p (pre "1.0.1.0 < 1.0.1.0.1")))
                (subsection "Comparison Procedures"
                    (p "In each of the following procedures, " (tt "VER1") 
                       " and " (tt "VER2") " may be either version records "
                       "or version strings.")
                    (group
                        (procedure "(version-compare VER1 VER2)"
                            (p "Returns " (tt "-1") " if " (tt "VER1") "<"
                               (tt "VER2") ", " (tt "0") " if " (tt "VER1") 
                               "=" (tt "VER2") ", and " (tt "1") " if "
                               (tt "VER1") ">" (tt "VER2") "."))
                        (definition
                            (signatures
                                (signature "procedure" "(version=? VER1 VER2)")
                                (signature "procedure" "(version<? VER1 VER2)")
                                (signature "procedure" "(version<=? VER1 VER2)")
                                (signature "procedure" "(version>=? VER1 VER2)")
                                (signature "procedure" "(version>? VER1 VER2)"))
                            (p "Comparison predicates for versions, according "
                               "to the rules given above."))
                        (procedure "(version-exact? VER1 VER2)"
                            (p "Equivalent to " (tt "(version=? VER1 VER2)")
                               "."))
                        (procedure "(version-older? VER1 VER2)"
                            (p "Equivalent to " (tt "(version<? VER1 VER2)")
                               "."))
                        (procedure "(version-newer? VER1 VER2)"
                            (p "Equivalent to " (tt "(version>? VER1 VER2)")
                               ".")))))

            (section "Sorting Procedures"
                (procedure "(version-sort VERSION-LIST [ASCENDING? #t])"
                    (p "Sorts " (tt "VERSION-LIST") " (a list of version "
                       "strings and/or records).  If " (tt "ASCENDING?") " is "
                       "given and " (tt "#f") ", " (tt "VERSION-LIST") " is "
                       "sorted in descending order.  " (tt "ASCENDING?") 
                       " defaults to " (tt "#t") ".")))

            (section "Bugs"
                (p "None known."))

            (section "Licence"
                (pre "Copyright (C) 2008, Lenny Frank.  All rights reserved.

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the Software),
to deal in the Software without restriction, including without limitation 
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the 
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED AS-IS, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE."))
        ))))


(eggdoc->html doc (elf-eggdoc-ss doc))
