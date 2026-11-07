(define-library (wqy24 define-empty-macros)
 (import (scheme base))
 (export define-empty-macros)
 (begin
  (define-syntax define-empty-macros
   (syntax-rules ()
    [(_ name ...) (begin (define-syntax name (syntax-rules ())) ...)]))))
