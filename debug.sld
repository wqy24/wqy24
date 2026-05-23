(define-library (wqy24 debug)
 (import (scheme base) (scheme write))
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
     (display msg p)
     (write-char #\space p)
     (write-shared obj p)]))))
