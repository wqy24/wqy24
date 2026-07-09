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

(define-library (wqy24 math)
 (import (scheme base) (scheme inexact))
 (export fmod sign shengjin)
 (begin
  (define (fmod a b)
   (- a (* b (truncate (/ a b)))))
  (define (sign a)
   (cond
    [(zero? a) 0]
    [(positive? a) 1]
    [else -1]))
  (define (shengjin a b c d) ; solve (= 0 (+ (* a (expt x 3)) (* b (expt x 2)) (* c x) d))
   (define (cbrt n)
    (if (negative? n)
     (- (expt (- n) (/ 1 3)))
     (expt n (/ 1 3))))
   (let* [[A (- (* b b) (* 3 a c))]
          [B (- (* b c) (* 9 a d))]
          [C (- (* c c) (* 3 b d))]
          [delta (- (* B B) (* 4 A C))]]
    (cond
     [(= A B 0) (vector (/ c b -1))]
     [(positive? delta)
      (let-values
        [[[Y1 Y2]
          (let [[Ab (* A b)] [sqdelta (sqrt delta)]]
           (values (+ Ab (* 1.5 a (- sqdelta B)))
                   (+ Ab (* -1.5 a (+ sqdelta B)))))]]
       (values (/ (+ b (cbrt Y1) (cbrt Y2)) a -3)))]
     [(zero? delta)
      (let [[K (/ B A)]]
       (values (- K (/ b a)) (/ K -2)))]
     [(negative? delta)
      (let* [[sqA (sqrt A)]
             [theta (acos (/ (- (* 2 A b) (* 3 a B))
                             2 (expt A (/ 3 2))))]
             [theta/3 (/ theta 3)]
             [sq3*sin_theta/3 (* (sqrt 3) (sin theta/3))]
             [cos_theta/3 (cos theta/3)]]
       (values
        (/ (- (* -2 sqA cos_theta/3) b) 3 a)
        (/ (- (* sqA (+ cos_theta/3 sq3*sin_theta/3)) b) 3 a)
        (/ (- (* sqA (- cos_theta/3 sq3*sin_theta/3)) b) 3 a)))])))))
