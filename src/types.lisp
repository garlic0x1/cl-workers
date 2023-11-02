(defpackage :cl-workers/types
  (:use :cl :cl-annot.class)
  (:export #:close-signal
           #:task-signal
           #:worker))
(in-package :cl-workers/types)
(annot:enable-annot-syntax)

;; ----------------------------------------------------------------------------
@export-slots
(defclass close-signal ()
  ((now?
    :initarg :now?
    :initform t
    :accessor now?
    :documentation "Do not process any more messages")))

;; ----------------------------------------------------------------------------
@export-accessors
(defclass task-signal ()
  ((message
    :initarg :message
    :initform nil
    :accessor message
    :documentation "Message to pass to worker")))

;; ----------------------------------------------------------------------------
@export-accessors
@export-slots
(defclass worker ()
  ((worker-name
    :initarg :name
    :initform (error ":name must be specified")
    :accessor worker-name
    :documentation "Hold the name of worker")
   (behavior
    :initarg :behavior
    :initform (error ":behav must be specified")
    :accessor behavior
    :documentation "Behavior")
   (messages
    :initform '()
    :accessor messages
    :documentation "Message stream sent to worker")
   ;; might need later
   (closed?
    :initform nil
    :accessor closed?
    :documentation "If closed, end worker")
   (worker-lock
    :initform (bt:make-lock)
    :accessor worker-lock
    :documentation "The lock is used when adding a message to the message queue")
   (worker-cv
    :initarg :cv
    :initform (bt:make-condition-variable)
    :accessor worker-cv
    :documentation "conditional variable used by the thread")
   (worker-thread
    :accessor worker-thread)))
