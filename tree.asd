;;;; tree.asd

(asdf:defsystem #:tree
  :description "Describe tree here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:sketch #:bst #:cl-collider #:pzmq)
  :components ((:file "package")
               (:file "sc-patch")
               (:file "audio" :depends-on ("sc-patch"))
               (:file "tree" :depends-on ("audio"))))
