#| vlws.sld -- this file is part of wqy24's scheme library set
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

(define-library (wqy24 vlws)
 (import (scheme base) (scheme case-lambda) (only (srfi 1) any))
 (export stream-cons stream-car stream-cdr stream-null stream? stream-null? stream-pair?
         stream-map stream-scan stream-take stream-drop stream stream->list list->stream
         stream-unfold stream-from stream-take-while stream-drop-while)
 (begin
  (define-syntax stream-cons
   (syntax-rules ()
    [(_ a d) (cons a (lambda () d))]))

  (define stream-car car)
  (define (stream-cdr s)
   (when (procedure? (cdr s)) (set-cdr! s ((cdr s))))
   (cdr s))

  (define stream-null '())
  (define stream-null? null?)
  (define stream-pair? pair?)

  (define (stream? o) (or (stream-pair? o) (stream-null? o)))

  (define-syntax stream
   (syntax-rules ()
    [(_) stream-null]
    [(_ e1 e2 ...) (stream-cons e1 (stream e2 ...))]))

  (define (realize s)
   (let loop [[c s]]
    (if (stream-null? c)
     (if (list? s) s (error "Stream->list failed" s))
     (loop (stream-cdr c)))))

  (define (list->stream l) l)

  (define stream->list
   (case-lambda
    [(s) (realize s)]
    [(n s) (realize (stream-take s n))]))

  (define (stream-map f . s)
   (if (any stream-null? s)
    stream-null
    (stream-cons (apply f (map stream-car s)) (apply stream-map f (map stream-cdr s)))))

  (define (stream-take s n)
   (unless (integer? n) (error "Taking non-integer:" n))
   (when (negative? n) (error "Taking negative:" n))
   (if (zero? n)
    stream-null
    (stream-cons (stream-car s)
     (stream-take (stream-cdr s) (- n 1)))))

  (define (stream-drop s n)
   (unless (integer? n) (error "Dropping non-integer:" n))
   (when (negative? n) (error "Dropping negative:" n))
   (if (zero? n) s (stream-drop (stream-cdr s) (- n 1))))

  (define (stream-scan proc base . strms)
   (let loop [[s strms] [prev base]]
    (stream-cons prev
     (if (any stream-null? s)
      stream-null
      (loop
       (map stream-cdr s)
       (apply proc prev (map stream-car s)))))))

  (define (stream-unfold f g seed)
   (stream-cons (f seed) (stream-unfold f g (g seed))))

  (define stream-from
   (case-lambda
    [(start) (stream-from start 1)]
    [(start step) (stream-unfold (lambda (x) x) (lambda (x) (+ x step)) start)]))

  (define (stream-take-while p? s)
   (if (and (stream-pair? s) (p? s))
    (stream-cons (stream-car s) (stream-take-while p? (stream-cdr s)))
    stream-null))

  (define (stream-drop-while p? s)
   (if (and (stream-pair? s) (p? s))
    (stream-drop-while p? (stream-cdr s))))))
