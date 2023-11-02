(asdf:defsystem "cl-workers"
  :description "Small additions to cl-actors"
  :version "0.1"
  :author "garlic0x1"
  :license "MIT"
  :depends-on (:bordeaux-threads :trivia)
  :components ((:file "workers")))
