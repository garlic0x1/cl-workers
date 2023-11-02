(defpackage :cl-workers
  (:use :cl :bordeaux-threads :cl-workers/types)
  (:import-from #:trivia :match)
  (:export #:defworker
           #:self
           #:send
           #:destroy-worker
           #:close-worker
           #:join-worker
           #:join-workers))
(in-package :cl-workers)

;; ----------------------------------------------------------------------------
(defmethod initialize-instance :after ((self worker) &key)
  "Uses the main functiona name to create a thread"
  (setf (worker-thread self)
        (bt:make-thread #'(lambda () (main self)) :name (worker-name self))))

;; ----------------------------------------------------------------------------
(defmethod send* ((self worker) msg)
  (with-slots (messages (lock worker-lock) (cv worker-cv)) self
    (with-lock-held (lock) (setf messages (nconc messages (list msg))))
    (condition-notify cv)))

;; ----------------------------------------------------------------------------
(defmethod send ((self worker) msg)
  "Send a value to the worker"
  (send* self (make-instance 'task-signal :message msg)))

;; ----------------------------------------------------------------------------
(defmethod destroy-worker ((self worker))
  "Stops the worker thread immediately"
  (destroy-thread (worker-thread self)))

;; ----------------------------------------------------------------------------
(defmethod close-worker ((self worker))
  "Close the worker"
  (send* self (make-instance 'close-signal)))

;; ----------------------------------------------------------------------------
;; The main which is started as a thread from the constructor I think that this
;; should be more of an internal function than a method (experiment with
;; funcallable-standard-class)
(defmethod main((self worker))
  (with-slots ((lock worker-lock) (cv worker-cv) behavior messages) self
    (loop (thread-yield)
          (with-lock-held (lock)
            (match (pop messages)
              ((task-signal :message msg) (funcall behavior msg))
              ((close-signal) (return))
              ((null) (condition-wait cv lock)))))))

;; ----------------------------------------------------------------------------
;; Macro for creating workers with the behavior specified by body
(defmacro defworker (name state vars &body body)
  `(defun ,name (&key (self) ,@state)
     (labels ((me ,(append vars ;; `(&key (next #'me next-supplied-p))
                    ) ,@body))
       (setf self (make-worker #'me ,(string name))) self)))

;; ----------------------------------------------------------------------------
;; The shell of a worker
(defun make-worker (behav name)
  (make-instance 'worker
                 :name (concatenate 'string "Worker: " name)
                 :behavior behav))

;; ----------------------------------------------------------------------------
(defmethod join-worker ((self worker))
  (bt:join-thread (cl-workers/types::worker-thread self)))

;; ----------------------------------------------------------------------------
(defun join-workers (&rest workers)
  (mapcar #'join-worker workers))

;; ----------------------------------------------------------------------------
;; Currying.
(defun curry (f &rest args)
  (lambda (&rest rem)
    (apply f (append rem args) )))
