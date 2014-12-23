#|
  /- filename -/
  load-star.scm

  /- copyright -/
  Copyright (c) 2014 Alexej Magura

  This file is part of Load*

  Load* is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Load* is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with load-star.scm.  If not, see <http://www.gnu.org/licenses/>.
|#

(module load* (load* depth)
  (import scheme chicken loop)
  
  (define (depth tree)
    (if (not-pair? tree) 0
      (+ 1 (apply max (map depth tree)))))

  (define (pathname-separator)
    (d

  (define (build-pathnames lst)
    (if (= (depth lst) 1)
      (map (lambda (lst)
             (let ((dir (car lst))
                   (files (cdr lst)))
               (map (lambda (file) (string-append dir
