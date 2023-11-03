(asdf:defsystem "cl-workers"
  :description "High level actor framework for Common Lisp"
  :version "0.1"
  :author "garlic0x1"
  :license "MIT"
  :depends-on (:alexandria
               :bordeaux-threads
               :queues.simple-cqueue
               :trivia)
  :components ((:module "src"
                :components
                ((:file "types")
                 (:file "core" :depends-on ("types"))))))
