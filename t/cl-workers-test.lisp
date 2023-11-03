(defpackage :cl-workers-test
  (:use :cl :cl-workers :fiveam))
(in-package :cl-workers-test)

(def-suite :cl-workers
  :description "Tests for cl-workers")
(in-suite :cl-workers)

;; ----------------------------------------------------------------------------
(test :counter
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
(test :recursion
  (let ((result))
    (defworker fac () (x ag)
      (setf result ag)
      (if (= x 0)
          (close-worker self)
          (send self (- x 1) (* x ag))))
    (let ((w (fac)))
      (send w 4 1)
      (join-worker w)
      (is (= 24 result)))))

;; ----------------------------------------------------------------------------
(test :waiting
  (let ((result))
    (defworker/global :sleepy () () (sleep 1) (setf result :done))
    (send :sleepy)
    (close-worker :sleepy)
    (join-worker :sleepy)
    (is (eql result :done))))

;; ----------------------------------------------------------------------------
(test :ordered
  (let ((result))
    (defworker/global :in-order () (x) (setf result (cons x result)))
    (send :in-order 1)
    (send :in-order 2)
    (send :in-order 3)
    (close-and-join-workers :in-order)
    (is (equal result '(3 2 1)))))
