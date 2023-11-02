(defpackage :cl-workers
  (:use :cl :bordeaux-threads)
  (:import-from #:trivia :match)
  (:export #:defworker
           #:self
           #:next
           #:behav
           #:send
           #:destroy-worker
           #:close-worker
           #:join-worker
           #:join-workers))
(in-package :cl-workers)

;; ----------------------------------------------------------------------------
(defclass close-signal ()
  ((now?
    :initarg :now?
    :initform t
    :accessor now?
    :documentation "Do not process any more messages")))

;; ----------------------------------------------------------------------------
(defclass task-signal ()
  ((message
    :initarg :message
    :initform nil
    :accessor message
    :documentation "Message to pass to worker")))

;; ----------------------------------------------------------------------------
(defclass worker ()
  ((name
    :initarg :name
    :initform (error ":name must be specified")
    :accessor name
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
   (lock
    :initform (bt:make-lock)
    :accessor lock
    :documentation "The lock is used when adding a message to the message queue")
   (cv
    :initarg :cv
    :initform (bt:make-condition-variable)
    :accessor cv
    :documentation "conditional variable used by the thread")
   (thread
    :accessor worker-thread)))

;; ----------------------------------------------------------------------------
(defmethod initialize-instance :after ((self worker) &key)
  "Uses the main functiona name to create a thread"
  (with-slots (name thread) self
    (setf thread (bt:make-thread #'(lambda() (main self)) :name name))))

;; ----------------------------------------------------------------------------
(defmethod send* ((self worker) msg)
  "
Creates a message sending thread which
1. Holds lock to the message (queue)
2. Appends messages (queue) with incoming message
3. Releases lock
4. Notifies the waiting thread that there is a message
"
  (with-slots (messages lock cv) self
    (with-lock-held (lock)
      (setf messages (nconc messages (list msg))))
    (condition-notify cv)
    ;; (bt:make-thread
    ;;  #'(lambda ()
    ;;      (with-lock-held (lock)
    ;;        (setf messages (nconc messages (list msg))))
    ;;      (condition-notify cv)))
    ))

(defmethod send ((self worker) msg)
  (send* self (make-instance 'task-signal :message msg)))

;; ----------------------------------------------------------------------------
(defmethod destroy-worker ((self worker))
  "Stops the worker thread"
  (with-slots (thread) self
    (destroy-thread  thread)))

;; ----------------------------------------------------------------------------
(defmethod close-worker ((self worker))
  "Close the worker"
  (send* self (make-instance 'close-signal)))

;; ----------------------------------------------------------------------------
;; (defun get-thread (worker) (first worker))
(defmethod get-thread ((self worker))
  "Returns the handle of a thread"
  (with-slots (thread) self
    thread))

;; ----------------------------------------------------------------------------
;; The main which is started as a thread from the constructor I think that this
;; should be more of an internal function than a method (experiment with
;; funcallable-standard-class)
(defmethod main((self worker))
  (with-slots (lock cv behavior messages) self
    (loop (thread-yield)
          (with-lock-held (lock)
            (match (pop messages)
              ((close-signal) (return))
              ((task-signal :message msg) (funcall behavior msg))
              ((type null) (condition-wait cv lock)))))))

;; ----------------------------------------------------------------------------
;; Create a behavior that can be attached to any worker
(defmacro behav (state vars  &body body)
  `(let ,state
     (labels ((me ,(append vars `(&key self  (next #'me next-supplied-p)))
                (setf next (curry next :self self))
             x   ,@body))
       #'me)))

;; ----------------------------------------------------------------------------
;; Macro for creating workers with the behavior specified by body
(defmacro defworker (name state vars &body body)
  `(defun ,name (&key (self) ,@state)
     (labels ((me ,(append vars `(&key (next #'me next-supplied-p)))
                (if next-supplied-p
                    (setf next (curry next :self self)))
                ,@body))
       (setf self (make-worker #'me ,(string name))) self)))

;; ----------------------------------------------------------------------------
;; The shell of a worker
(defun make-worker (behav name)
  (make-instance 'worker
                 :name (concatenate 'string "Worker: " name)
                 :behavior behav))

;; ----------------------------------------------------------------------------
(defmethod join-worker ((self worker))
  (bt:join-thread (worker-thread self)))

;; ----------------------------------------------------------------------------
(defun join-workers (&rest workers)
  (mapcar #'join-worker workers))

;; ----------------------------------------------------------------------------
;; Currying.
(defun curry (f &rest args)
  (lambda (&rest rem)
    (apply f (append rem args) )))

;; ----------------------------------------------------------------------------
;; Scratch.

;; (defmacro defworker-test (name state args &body body)
;;   `(defactor ,name ,(cons '(closed? nil) state) ,args
;;      ,@body
;;      (unless (and closed? (not (messages self)))
;;          next)))


;; (defworker-test printer2 () (msg)
;;   (sleep 1)
;;   (when (eql msg :close) (setf closed? t))
;;   (print msg)
;;   (force-output))
