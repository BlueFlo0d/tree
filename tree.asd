;;;; tree.asd

(asdf:defsystem #:tree
  :description "Describe tree here"
  :author "Qiantan Hong <qhong@mit.edu>"
  :license  "GNU Affero General Public License"
  :version "0.0.1"
  :serial t
  :depends-on (#:sketch #:bst #:cl-collider #:pzmq #:local-time)
  :components ((:file "package")
               (:file "sc-patch")
               (:file "audio" :depends-on ("sc-patch"))
               (:file "tree" :depends-on ("audio"))))
