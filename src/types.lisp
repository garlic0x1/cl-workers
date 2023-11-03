(defpackage :cl-workers/types
  (:use :cl)
  (:export #:worker-signal
           #:close-signal
           #:task-signal
           #:signal-message
           #:worker
           #:worker-name
           #:worker-behav
           #:worker-queue
           #:worker-lock
           #:worker-cv
           #:worker-thread))
(in-package :cl-workers/types)

;; ----------------------------------------------------------------------------
(defclass worker-signal () ())

;; ----------------------------------------------------------------------------
(defclass close-signal (worker-signal) ())

;; ----------------------------------------------------------------------------
(defclass task-signal (worker-signal)
  ((message :initarg :message
            :accessor signal-message)))

;; ----------------------------------------------------------------------------
(defclass worker ()
  ((name   :initarg :name
           :initform (error ":name must be specified")
           :accessor worker-name)
   (behav  :initarg :behav
           :initform (error ":behav must be specified")
           :accessor worker-behav)
   (queue  :initform (queues:make-queue :simple-cqueue)
           :accessor worker-queue)
   (lock   :initform (bt:make-lock)
           :accessor worker-lock)
   (cv     :initform (bt:make-condition-variable)
           :accessor worker-cv)
   (thread :accessor worker-thread)))
