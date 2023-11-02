(defpackage :cl-workers
  (:use :cl :bordeaux-threads :cl-workers/types)
  (:import-from #:trivia :match)
  (:export #:defworker
           #:self
           #:send
           #:destroy-worker
           #:close-worker
           #:join-worker
           #:join-workers
           #:close-and-join-workers
           #:worker-messages
           #:spawn-worker
           #:*named-workers*))
(in-package :cl-workers)

;; ----------------------------------------------------------------------------
(defvar *named-workers* (make-hash-table))

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
(defmethod send* ((self symbol) msg)
  (send* (gethash self *named-workers*) msg))

;; ----------------------------------------------------------------------------
(defun send (worker &rest args)
  "Send a value to the worker"
  (send* worker (make-instance 'task-signal :message args)))

;; ----------------------------------------------------------------------------
(defmethod close-worker (worker)
  "Close the worker"
  (send* worker (make-instance 'close-signal)))

;; ----------------------------------------------------------------------------
(defmethod destroy-worker ((self worker))
  "Stops the worker thread immediately"
  (destroy-thread (worker-thread self)))

;; ----------------------------------------------------------------------------
(defmethod join-worker ((self worker))
  (bt:join-thread (cl-workers/types::worker-thread self)))

;; ----------------------------------------------------------------------------
(defmethod close-and-join-workers (&rest workers)
  (loop :for w :in workers
        :do (when (symbolp w) (setf w (gethash w *named-workers*)))
        :do (close-worker w)
        :do (join-worker w)))

;; ----------------------------------------------------------------------------
;; The main which is started as a thread from the constructor I think that this
;; should be more of an internal function than a method (experiment with
;; funcallable-standard-class)
(defmethod main((self worker))
  (with-slots ((lock worker-lock) (cv worker-cv) behavior messages) self
    (loop (thread-yield)
          (with-lock-held (lock)
            (match (pop messages)
              ((task-signal :message msg) (apply behavior msg))
              ((close-signal) (return))
              ((null) (condition-wait cv lock)))))))

;; ----------------------------------------------------------------------------
;; Macro for creating workers with the behavior specified by body
(defmacro defworker (name state vars &body body)
  `(defun ,name (&key (self) ,@state)
     (labels ((me ,vars ,@body))
       (setf self (make-worker #'me ,(string name)))
       self)))

;; ----------------------------------------------------------------------------
;; Macro for spawning global singleton workers with the behavior specified by body
(defmacro spawn-worker (name state vars &body body)
  `(setf (gethash ,name *named-workers*)
         (let ,state
           (labels ((me ,vars ,@body))
             (setf self (make-worker #'me ,(string name)))
             self))))

;; (spawn-worker *mailer* () (msg) (print msg))

;; ----------------------------------------------------------------------------
;; The shell of a worker
(defun make-worker (behav name)
  (make-instance 'worker
                 :name (concatenate 'string "Worker: " name)
                 :behavior behav))

;; ----------------------------------------------------------------------------
;; Currying.
(defun curry (f &rest args)
  (lambda (&rest rem)
    (apply f (append rem args) )))
