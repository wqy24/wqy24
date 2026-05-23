(define-library (wqy24 log)
 (import (scheme base) (scheme write))
 (export log default-log-port)
 (begin
  (define default-log-port
   (make-parameter (current-output-port)
    (lambda (p) p)))
  (define log
   (case-lambda
    [(msg obj)
     (log msg obj (default-log-port))]
    [(msg obj o)
     (display msg p)
     (write-char #\space p)
     (write-shared obj p)]))))
