(defpackage :cl-workers
  (:use :cl :cl-workers/types)
  (:import-from #:trivia :match)
  (:import-from #:alexandria :curry)
  (:export #:*global-workers*
           #:send
           #:self
           #:close-worker
           #:join-worker
           #:destroy-worker
           #:close-and-join-workers
           #:defworker
           #:defworker/global

           ;; re-export class methods
           #:worker
           #:worker-name
           #:worker-behav
           #:worker-queue
           #:worker-lock
           #:worker-cv
           #:worker-thread
           #:worker-open?
           #:worker-store))
(in-package :cl-workers)

;; ----------------------------------------------------------------------------
(defvar *global-workers* (make-hash-table))

;; ----------------------------------------------------------------------------
(defmethod send-signal ((obj worker) (msg worker-signal))
  "Send a signal type to an actor and notify it"
  (unless (worker-open? obj)
    (warn (format nil "Worker ~w is closed" obj))
    (return-from send-signal))
  (queues:qpush (worker-queue obj) msg)
  (bt2:condition-notify (worker-cv obj)))

;; ----------------------------------------------------------------------------
(defgeneric send (obj &rest args)
  (:documentation "Send a message to an actor")
  (:method ((obj worker) &rest args)
    (send-signal obj (make-instance 'task-signal :message args)))
  (:method ((obj symbol) &rest args)
    (send-signal (gethash obj *global-workers*)
                 (make-instance 'task-signal :message args))))

;; ----------------------------------------------------------------------------
(defgeneric close-worker (obj)
  (:documentation "Send a close-signal to an actor")
  (:method ((obj worker))
    (send-signal obj (make-instance 'close-signal))
    (setf (worker-open? obj) nil))
  (:method ((obj symbol)) (close-worker (gethash obj *global-workers*))))

;; ----------------------------------------------------------------------------
(defgeneric join-worker (obj)
  (:documentation "Wait for an actor to finish working")
  (:method ((obj worker))
    (bt2:join-thread (worker-thread obj))
    (worker-store obj))
  (:method ((obj symbol))
    (join-worker (gethash obj *global-workers*))))

;; ----------------------------------------------------------------------------
(defgeneric destroy-worker (obj)
  (:documentation "Immediately destroy an actor's thread")
  (:method ((obj worker)) (bt2:destroy-thread (worker-thread obj)))
  (:method ((obj symbol)) (destroy-worker (gethash obj *global-workers*))))

;; ----------------------------------------------------------------------------
(defun close-and-join-workers (&rest workers)
  (mapcar #'close-worker workers)
  (mapcar #'join-worker workers))

;; ----------------------------------------------------------------------------
(defmethod handle-message ((obj worker) (msg list))
  "Apply message to actor behavior"
  (setf (worker-store obj) (apply (worker-behav obj) msg)))

;; ----------------------------------------------------------------------------
(defmethod start-worker ((obj worker))
  "Main event loop for actors"
  (loop (bt2:thread-yield)
        (match (queues:qpop (worker-queue obj))
          ((task-signal :message msg) (handle-message obj msg))
          ((close-signal) (return (worker-store obj)))
          ((null) (bt2:with-lock-held ((worker-lock obj)) (bt2:condition-wait (worker-cv obj) (worker-lock obj)))))))

;; ----------------------------------------------------------------------------
(defun make-worker (name behav)
  "Make instance and start event loop"
  (let ((worker (make-instance 'worker :name name :behav behav)))
    (setf (worker-thread worker) (bt2:make-thread (lambda () (start-worker worker)) :name name))
    worker))

;; ----------------------------------------------------------------------------
(defmacro with-behavior (name state vars &body body)
  `(let ,(cons '(self nil) state)
     (labels ((me ,vars ,@body))
       (setf self (make-worker ,(string name) #'me)))))

;; ----------------------------------------------------------------------------
(defmacro defworker (name state vars &body body)
  `(defun ,name () (with-behavior ,name ,state ,vars ,@body)))

;; ----------------------------------------------------------------------------
(defmacro defworker/global (name state vars &body body)
  `(setf (gethash ,name *global-workers*)
         (with-behavior ,name ,state ,vars ,@body)))

;; ----------------------------------------------------------------------------
;; considered automatically differentiating between defworker and defworker/global
;; probably not as clear to the user though...
;; (defmacro defworker (name state vars &body body)
;;   (let ((behav `(with-behavior ,name ,state ,vars ,@body)))
;;     (if (keywordp name)
;;         `(setf (gethash ,name *global-workers*) ,behav)
;;         `(defun ,name () behav))))
