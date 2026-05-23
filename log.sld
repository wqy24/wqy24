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
    [(msg obj)
     (display msg)
     (write-char #\space)
     (write-shared obj)]))))
