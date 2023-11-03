(asdf:defsystem "cl-workers-test"
  :depends-on (:fiveam :cl-workers)
  :components ((:module "t"
                :components ((:file "cl-workers-test")))))
