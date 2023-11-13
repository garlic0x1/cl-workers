(defpackage :cl-workers-test
  (:use :cl :cl-workers :fiveam))
(in-package :cl-workers-test)

(def-suite :cl-workers
  :description "Tests for cl-workers")
(in-suite :cl-workers)

;; ----------------------------------------------------------------------------
(test :counter-lexical
  (let ((result))
    (defworker/global :counter ((i 0)) (increment)
      (incf i increment)
      (setf result i))
    (send :counter 1)
    (send :counter 2)
    (send :counter 3)
    (send :counter 4)
    (close-and-join-workers :counter)
    (is (= 10 result))))

;; ----------------------------------------------------------------------------
(test :counter
  (defworker/global :counter ((i 0)) (x)
    (incf i x))
  (send :counter 2)
  (send :counter 3)
  (close-worker :counter)
  (is (= 5 (join-worker :counter))))

;; ----------------------------------------------------------------------------
(test :recursion-lexical
  (let ((result))
    (defworker fac () (x ag)
      (if (= x 0)
          (progn (setf result ag) (close-worker self))
          (send self (- x 1) (* x ag))))
    (let ((w (fac)))
      (send w 4 1)
      (join-worker w)
      (is (= 24 result)))))

;; ----------------------------------------------------------------------------
(test :recursion
  (defworker/global :fac () (x ag)
    (if (= x 0)
        (progn (close-worker self) ag)
        (send self (- x 1) (* x ag))))
  (send :fac 4 1)
  (is (= 24 (join-worker :fac))))

;; ----------------------------------------------------------------------------
(test :waiting

  ;; create singleton worker
  (defworker/global :sleepy () (msg)
    (sleep 1)
    (print msg)
    :done)
  (send :sleepy "print me")              ; send task signal
  (close-worker :sleepy)                 ; send close signal
  (is (eql :done (join-worker :sleepy))) ; check result

  )

;; ----------------------------------------------------------------------------
(test :ordered-with-store
  (defworker in-order ((ag)) (x) (setf ag (cons x ag)))
  (let ((w (in-order)))
    (send w 1)
    (send w 2)
    (send w 3)
    (close-worker w)
    (is (equal (join-worker w) '(3 2 1)))))
