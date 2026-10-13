(define-library (wqy24 syntax-fill)
 (export syntax-fill identifier? identifier=? identifier->symbol identifier-eq?)
 (import (scheme base) (scheme cxr))
 (cond-expand
  [chibi (import (only (chibi) er-macro-transformer identifier? identifier->symbol))]
  [gauche (import (only (gauche base) er-macro-transformer identifier? identifier->symbol))])
 (define comparer (make-parameter #f))
 (define (identifier=? id1 id2)
  (define c (comparer))
  (if c
   (c id1 id2)
   (error "Attempt to use identifier=? out of syntax-fill" id1 id2)))
 (define (identifier-eq? id1 id2)
  (unless (identifier? id1) (error "Not an identifier" id1))
  (unless (identifier? id2) (error "Not an identifier" id2))
  (eq? id1 id2))
 (define (pvar-name n)
  (string->symbol
   (string-append "syntax-fill-pv-" (number->string n))))
 (define (replace-unquotes templt uq pvs)
  (define (walk t)
   (cond
    [(vector? t) (vector-map walk t)]
    [(pair? t)
     (if (and (list? t)
              (= (length t) 2)
              (identifier=? (car t) uq))
         (if (integer? (cadr t))
             (list-ref pvs (cadr t))
             (cadr t))
         (cons (walk (car t)) (walk (cdr t))))]
    [else t]))
  (walk templt))
 (define-syntax syntax-fill
  (syntax-rules (=>)
   [(_ [[bdname form] ...] code ... => template)
    (begin
     (define-syntax helper
      (er-macro-transformer
       (lambda (e r c)
        (parameterize [[comparer c]]
         (define templt (cadr e))
         (define arglist (caddr e))
         (define idpack (cdddr e))
         (define uq (car idpack))
         (define $define-syntax (cadr idpack))
         (define $begin (caddr idpack))
         (define $syntax-rules (cadddr idpack))
         (define $_ (cadr (cdddr idpack)))
         (define fillings
          (call-with-values
           (lambda ()
            (apply (lambda (bdname ...) code ...) arglist))
           list))
         (define pvars
          (let loop [[i 0] [acc '()]]
           (if (= i (length fillings))
               (reverse acc)
               (loop (+ i 1) (cons (r (pvar-name i)) acc)))))
         (define new-template (replace-unquotes templt uq pvars))
         `(,$begin
           (,$define-syntax ,(r 'm)
            (,$syntax-rules ()
             [[,$_ ,@pvars] ,new-template]))
           (,(r 'm) ,@fillings))))))
     (helper template (form ...) unquote define-syntax begin syntax-rules _))])))