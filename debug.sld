#|  -- this file is part of wqy24's scheme library set
 | Copyright (C) 2026 wqy24
 |
 | This set is free software: you can redistribute it and/or modify
 | it under the terms of the GNU General Public License as published by
 | the Free Software Foundation, either version 3 of the License, or
 | (at your option) any later version.
 |
 | This set is distributed in the hope that it will be useful,
 | but WITHOUT ANY WARRANTY; without even the implied warranty of
 | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 | GNU General Public License for more details.
 |
 | You should have received a copy of the GNU General Public License
 | along with this set. If not, see <https://www.gnu.org/licenses/>.
 |#

(define-library (wqy24 debug)
 (import (scheme base) (scheme case-lambda) (scheme write))
 (export debug default-log-port)
 (begin
  (define default-log-port
   (make-parameter (current-output-port)
    (lambda (p) p)))
  (define debug
   (case-lambda
    [(msg obj)
     (debug msg obj (default-log-port))]
    [(msg obj p)
     (display "[DEBUG] ")
     (display msg p)
     (write-char #\: p)
     (write-shared obj p)
     (newline)
     obj]))))
