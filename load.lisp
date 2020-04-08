(pushnew (make-pathname :directory (pathname-directory *load-pathname*)) asdf:*central-registry*)
(asdf:load-system :tree)
(in-package :tree)
(make-instance 'tree)
