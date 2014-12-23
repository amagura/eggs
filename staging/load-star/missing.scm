#|
  /- filename -/
  missing.scm

  /- copyright -/
  Copyright (c) 2014 Alexej Magura

  missing.scm is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  missing.scm is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with missing.scm.  If not, see <http://www.gnu.org/licenses/>.
|#

(module missing (depth 1+ 1- !* false? true?)
  (import scheme chicken srfi-1)
  
  (define (1+ n) (+ 1 n))
  (define (1- n) (- 1 n))
  
  (define (!* fn #!rest ...)
    (not (eval `(,fn ,@...))))
  
  (define (false? exp)
    (not (and exp #t)))
  
  (define (true? exp)
    (and exp #t))

  (define (depth tree)
    (if (not-pair? tree) 0
      (1+ (apply max (map depth tree))))))
