(pushnew (ccl:current-directory) ql:*local-project-directories*)
(declaim (optimize (debug 3)))
(unless (boundp '*cli-player-id*)
  (format t "Please specifiy player ID:")
  (setq *cli-player-id* (read))
  (format t "~%"))

(ql:quickload "tree")
(in-package :tree)
(make-instance 'tree :player-id cl-user::*cli-player-id*)
