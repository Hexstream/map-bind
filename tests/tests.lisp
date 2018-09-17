(cl:defpackage #:map-bind_tests
  (:use #:cl #:parachute)
  (:import-from #:map-bind #:map-bind))

(cl:in-package #:map-bind_tests)

(define-test "mapcar, one argument, one list"
  (is equal
      (map-bind (mapcar) ((number '(1 2 3)))
        (1+ number))
      '(2 3 4)))

(define-test "funcall mapcar, one argument, one list"
  :depends-on ("mapcar, one argument, one list")
  (is equal
      (map-bind (funcall #'mapcar) ((number '(1 2 3)))
        (1+ number))
      '(2 3 4)))

(define-test "apply mapcar, one argument, one list"
  :depends-on ("funcall mapcar, one argument, one list")
  (is equal
      (map-bind (apply #'mapcar) ((number (list '(1 2 3))))
        (1+ number))
      '(2 3 4)))

(define-test "featured-complex-example"
  (is equalp
      (map-bind (multiple-value-call #'map 'vector)
          ((symbol #(a b c))
           (number '(1 2 3))
           ((&rest others &key &allow-other-keys) (values '(d e f) #(4 5 6)))
           ((&aux (plus-ten (+ number 10)))))
        (list (1- number) symbol plus-ten (reverse others)))
      #((0 A 11 (4 D)) (1 B 12 (5 E)) (2 C 13 (6 F)))))
