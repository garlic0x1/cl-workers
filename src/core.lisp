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
           #:worker-store))
(in-package :cl-workers)

;; ----------------------------------------------------------------------------
(defvar *global-workers* (make-hash-table))

;; ----------------------------------------------------------------------------
(defmethod send-signal ((obj worker) (msg worker-signal))
  (queues:qpush (worker-queue obj) msg)
  (bt:condition-notify (worker-cv obj)))

;; ----------------------------------------------------------------------------
(defgeneric send (obj &rest args)
  (:method ((obj worker) &rest args)
    (send-signal obj (make-instance 'task-signal :message args)))
  (:method ((obj symbol) &rest args)
    (send-signal (gethash obj *global-workers*)
                 (make-instance 'task-signal :message args))))

;; ----------------------------------------------------------------------------
(defgeneric close-worker (obj)
  (:method ((obj worker)) (send-signal obj (make-instance 'close-signal)))
  (:method ((obj symbol)) (close-worker (gethash obj *global-workers*))))

;; ----------------------------------------------------------------------------
(defgeneric join-worker (obj)
  (:method ((obj worker))
    (bt:join-thread (worker-thread obj))
    (worker-store obj))
  (:method ((obj symbol))
    (join-worker (gethash obj *global-workers*))))

;; ----------------------------------------------------------------------------
(defgeneric destroy-worker (obj)
  (:method ((obj worker)) (bt:destroy-thread (worker-thread obj)))
  (:method ((obj symbol)) (destroy-worker (gethash obj *global-workers*))))

;; ----------------------------------------------------------------------------
(defun close-and-join-workers (&rest workers)
  (mapcar #'close-worker workers)
  (mapcar #'join-worker workers))

;; ----------------------------------------------------------------------------
(defmethod start-worker ((obj worker))
  (let ((lock (worker-lock obj))
        (cv (worker-cv obj))
        (behav (worker-behav obj)))
    (loop (bt:thread-yield)
          (match (queues:qpop (worker-queue obj))
            ((task-signal :message msg) (setf (worker-store obj) (apply behav msg)))
            ((close-signal) (return (worker-store obj)))
            ((null) (bt:with-lock-held (lock) (bt:condition-wait cv lock)))))))

;; ----------------------------------------------------------------------------
(defun make-worker (name behav)
  (let ((worker (make-instance 'worker :name name :behav behav)))
    (setf (worker-thread worker) (bt:make-thread (lambda () (start-worker worker)) :name name))
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
;; (defmacro defworker (name state vars &body body)
;;   (let ((behav `(with-behavior ,name ,state ,vars ,@body)))
;;     (if (keywordp name)
;;         `(setf (gethash ,name *global-workers*) ,behav)
;;         `(defun ,name () behav))))
