(defpackage :cl-workers-test
  (:use :cl :cl-workers :fiveam))
(in-package :cl-workers-test)

(def-suite :cl-workers
  :description "Tests for cl-workers")
(in-suite :cl-workers)

(defworker test-worker () (msg)
  (sleep 1)
  (print (cl-workers::messages self))
  (print msg)
  (force-output))



(test :basic
  (setf w (test-worker))
  (send w "hi")
  (send w "hi")
  (send w "hi")
  (send w "hi")
  (close-worker w)
  (send w "bye")
  (send w "bye")
  (send w "bye")
  (print "sent")
  (join-worker w)
  )
