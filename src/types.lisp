(uiop:define-package :cl-workers/types
  (:use :cl :cl-annot.class))
(in-package :cl-workers/types)
(cl-annot:enable-annot-syntax)

;; ----------------------------------------------------------------------------
@export-class
(defclass worker-signal () ())

;; ----------------------------------------------------------------------------
@export-class
(defclass close-signal (worker-signal) ())

;; ----------------------------------------------------------------------------
@export-class
(defclass task-signal (worker-signal)
  ((message :initarg :message
            :accessor signal-message)))

;; ----------------------------------------------------------------------------
@export-class
(defclass error-signal (worker-signal)
  ((message :initarg :message
            :accessor signal-message)))

;; ----------------------------------------------------------------------------
@export-class
(defclass worker ()
  ((name   :initarg :name
           :initform (error ":name must be specified")
           :accessor worker-name)
   (behav  :initarg :behav
           :initform (error ":behav must be specified")
           :accessor worker-behav)
   (queue  :initform (queues:make-queue :simple-cqueue)
           :accessor worker-queue)
   (open?  :initform t 
           :accessor worker-open?)
   (store  :initarg store
           :initform nil
           :accessor worker-store)
   (lock   :initform (bt2:make-lock)
           :accessor worker-lock)
   (cv     :initform (bt2:make-condition-variable)
           :accessor worker-cv)
   (thread :accessor worker-thread)))
