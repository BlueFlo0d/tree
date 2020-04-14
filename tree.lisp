;;; tree.lisp

(in-package #:tree)

(defstruct channel
  (synth 'sawsynth)
  (amp 0.2)
  (elevation 0.0)
  (color '(0 0.5 0.9)))
(defparameter *channel-settings*
  `#(,(make-channel)
     ,(make-channel
       :synth 'plucksynth
       :color '(0.5 0.0 0.9))))
;;; UI
(defparameter last-gen-id 0)
(defun gen-id (player-id)
  (incf last-gen-id)
  (cons player-id last-gen-id))
(defstruct node
  (x 0 :type real)
  (y 0 :type real)
  (children nil :type list)
  (parent nil)
  (channel 0)
  (intensity 0.2)
  (decay 0.2)
  (pan 0.0)
  (reverb 0.5)
  (continuation nil)
  (mute nil)
  (id '(0 . 0)))
(defstruct node-base
  (nodes nil)
  (idtable (make-hash-table :test 'equal)))
(defparameter *window-width* 1920)
(defparameter *window-height* 1080)

(defparameter *logical-width* 8)
(defparameter *logical-height* 4)
(defun logical-to-window-x (x)
  (round (* (/ x *logical-width*) *window-width*)))
(defun logical-to-window-y (y)
  (round (* (- 1 (/ y *logical-height*)) *window-height*)))
(defun window-to-logical-x (x)
  (* (/ x *window-width*) *logical-width*))
(defun window-to-logical-y (y)
  (* (- 1 (/ y *window-height*)) *logical-height*))
(defun node-base-add (node-base node)
  (push node (node-base-nodes node-base))
  (when (node-parent node)
    (push node (node-children (node-parent node))))
  (setf (gethash (node-id node) (node-base-idtable node-base))
        node)
  node-base)
(defun node-base-delete (node-base node)
  (when (node-parent node) ;; don't delete root node!
    (symbol-macrolet ((parent-place (node-children (node-parent node)))
                      (base-place (node-base-nodes node-base)))
      (setf parent-place (delete node parent-place))
      (labels ((process-node (node)
                 (setf (node-parent node) nil)
                 (mapc #'process-node (node-children node))))
        (process-node node))
      (setf base-place
            (delete-if (lambda (node)
                         (unless (or (node-parent node)
                                     (and (= (node-x node) 0)
                                          (= (node-y node) 1)))
                           (remhash (node-id node) (node-base-idtable node-base))))
                       base-place)))))
(defun node-base-dump (node-base)
  (mapcar (lambda (n)
            (list
             (node-id n)
             (when
                 (node-parent n)
               (node-id (node-parent n)))
             (node-x n)
             (node-y n)
             (node-channel n)
             (node-intensity n)
             (node-decay n)
             (node-pan n)
             (node-reverb n)
             (node-continuation n)
             (node-mute n)))
          (node-base-nodes node-base)))
(defun node-base-load (list)
  (let ((node-base (make-node-base)))
    (mapc (lambda (item)
            (destructuring-bind (id parent-id x y channel intensity decay pan reverb continuation mute) item
              (declare (ignore parent-id))
              (let ((new-node (make-node
                               :id id
                               :x x
                               :y y
                               :channel channel
                               :intensity intensity
                               :decay decay
                               :pan pan
                               :reverb reverb
                               :continuation continuation
                               :mute mute)))
                (push new-node (node-base-nodes node-base))
                (setf (gethash id (node-base-idtable node-base)) new-node))))
          list)
    (mapc (lambda (item)
            (destructuring-bind (id parent-id &rest things) item
              (declare (ignore things))
              (let ((child (gethash id (node-base-idtable node-base)))
                    (parent (gethash parent-id (node-base-idtable node-base))))
                (setf (node-parent child) parent)
                (when parent
                  (push child (node-children parent))))))
          list)
    node-base))
(defun node-move-to (node target-x target-y)
  (let ((delta-x (- target-x (node-x node)))
        (delta-y (/ target-y (node-y node))))
    (labels ((process-node (node)
               (incf (node-x node) delta-x)
               (setf (node-y node) (* delta-y (node-y node)))
               (mapc #'process-node (node-children node))))
      (process-node node))))
(defun trim-value (value)
  (if (> value 1.0)
      1.0
      (if (< value 0.0)
          0.0
          value)))
(defun node-change-param (node param delta &optional (trim-function
                                                      #'trim-value))
  (labels ((process-node (node)
             (setf (slot-value node param)
                   (funcall trim-function (+ delta (slot-value node param))))
             (mapc #'process-node (node-children node))))
    (process-node node)))

(defun find-nearest-node (node-base x y)
  (let ((nearest-distance (+ *logical-width* *logical-height*))
        (nearest-node))
    (mapc (lambda (node)
            (let ((my-distance (+ (abs (- x (node-x node)))
                                  (abs (- y (node-y node))))))
              (when (< my-distance nearest-distance)
                (setq nearest-node node)
                (setq nearest-distance my-distance))))
          (node-base-nodes node-base))
    nearest-node))
(defun find-swept-nodes (node-base x-start x-end)
  (when (< x-end x-start)
    (setq x-start (- x-start *logical-width*)))
  (remove-if-not (lambda (node)
                   (and (>= (node-x node) x-start)
                        (< (node-x node) x-end)))
                 (node-base-nodes node-base)))
(defun get-candidate-coordinate (x0 y0 x y)
  (flet ((candidate-for-division (x0 x division)
           (let* ((parts (round (* (- x x0) division)))
                  (xn (/ parts division))
                  (xc (+ x0 xn)))
             (values xc parts)))
         (candidate-for-log-division (x0 x division)
           (let* ((parts-1 (round (* (/ x x0) division)))
                  (parts (if (> parts-1 0)
                             parts-1 1))
                  (xn (/ parts division))
                  (xc (* x0 xn)))
             (values xc (abs (- xc x))))))
    (let ((xc) (yc) (yerr *logical-height*))
      (multiple-value-bind (x4c x4p) (candidate-for-division x0 x 4)
        (setq xc (if (> (abs x4p) 1) x4c
                     (nth-value 0 (candidate-for-division x0 x 16)))))
      (setq yc y0)
      (mapc (lambda (division)
              (multiple-value-bind (ync yne)
                  (candidate-for-log-division y0 y division)
                (when (< yne yerr)
                  (setq yerr yne)
                  (setq yc ync))))
            '(3 4 5 6 7))
      (mapc (lambda (division)
              (multiple-value-bind (ync yne)
                  (candidate-for-log-division 1 y division)
                (when (< yne yerr)
                  (setq yerr yne)
                  (setq yc ync))))
            '(2 3))
      (values xc yc))))
(defun crosshair (wx wy)
  (line wx (- wy 4) wx (+ wy 6))
  (line (- wx 5) wy (+ wx 5) wy))
(local-time:enable-read-macros)
(defparameter *last-broadcast-time* (local-time:now))
(defstruct cursor
  (x 0)
  (y 0)
  (state :insert)
  (edit-parameter 'intensity)
  (logical-candidate-x)
  (logical-candidate-y)
  (base-node)
  (selected-node)
  (selected-channel 0))
(defparameter *current-player-id* 0)
(defstruct activation
  (synth)
  (start)
  (end))
(defun node-color-list (node)
  (let ((color (channel-color
                (aref *channel-settings* (node-channel node)))))
    (when (node-mute node)
      (setq color (mapcar (lambda (x) (/ (+ x 1.5) 3)) color)))
    color))
(defsketch tree
    ((title "Just Tree (loading...)")
     (width *window-width*)
     (height *window-height*)
     (initial-time (local-time:now))
     (tape-x 0)
     (node-base (node-base-add (make-node-base) (make-node :y 1)))
     (cursors
      (let ((new-cursors (make-hash-table)))
        (setf (gethash 0 new-cursors) (make-cursor))
        new-cursors))
     (activations nil)
     (player-id 0)
     (socket)
     (side-socket))
  ;; (setq *window-width* width)
  ;; (setq *window-height* height)
  (background (gray 0.9))
  ;; network
  (if (= player-id 0)
      (progn
        (let ((current-time (local-time:now)))
          (when (> (local-time:timestamp-difference current-time *last-broadcast-time*) 1)
            (setq *last-broadcast-time* current-time)
            (pzmq:send socket (write-to-string (cons initial-time (node-base-dump node-base))))))
        (handler-case
            (loop for i from 1
                  do (let* ((msg (pzmq:recv-string side-socket :dontwait t))
                            (list (read-from-string msg)))
                       (pzmq:send socket msg)
                       (apply (symbol-function (car list))
                              (cons sketch::instance (cdr list)))))
          (pzmq:eagain ())))
      (progn
        (handler-case
            (loop for i from 1
                  do (let* ((msg (pzmq:recv-string socket :dontwait t))
                            (list (read-from-string msg)))
                       (typecase (car list)
                         (local-time:timestamp
                          (setq initial-time (car list))
                          (setq node-base
                                (node-base-load (cdr list)))
                          (maphash
                           (lambda (key cursor)
                             (with-slots (base-node selected-node) cursor
                               (when base-node
                                 (setq base-node (gethash (node-id base-node) (node-base-idtable node-base))))
                               (when selected-node
                                 (setq selected-node (gethash (node-id selected-node) (node-base-idtable node-base))))))
                           cursors))
                         (symbol
                          (when (not (= (cadr list) player-id))
                            (apply (symbol-function (car list))
                                   (cons sketch::instance (cdr list))))))))
          (pzmq:eagain ()))))
  ;; grid
  (with-pen (make-pen :stroke (rgb 0.5 0.5 1.0 0.4))
    (loop for i from 1 below *logical-width*
          do (let ((wx (logical-to-window-x i)))
               (line wx 0 wx *window-height*)))
    (loop for i from 1 below *logical-height*
          do (let ((wy (logical-to-window-y i)))
               (line 0 wy *window-width* wy))))
  ;; tape
  (let ((new-tape-x
          (* width (mod (/ (local-time:timestamp-difference (local-time:now) initial-time) 12.0) 1.0))))
    (mapc (lambda (node)
            (let ((channel (aref *channel-settings* (node-channel node))))
              (unless (node-mute node)
                (synth (channel-synth
                        channel)
                       :freq (* (node-y node) 55.0)
                       :amp (channel-amp channel)
                       :pan (/ (node-pan node) pi)
                       :intensity (node-intensity node)
                       :decay (node-decay node)
                       :reverb (node-reverb node)))))
          (find-swept-nodes node-base
                            (window-to-logical-x tape-x)
                            (window-to-logical-x new-tape-x)))
    (setq tape-x new-tape-x))
  (with-pen (make-pen :stroke (rgb 0 0.5 0.9 0.4))
    (line tape-x 0 tape-x height))
  ;; draw node-base
  (with-pen (make-pen :stroke (gray 0.8))
    (mapc (lambda (node)
            (let ((wx (logical-to-window-x
                       (node-x node)))
                  (wy (logical-to-window-y
                       (node-y node)))
                  (radius (+ 10
                             (* 20 (node-decay node))))
                  (color (node-color-list node)))
              (with-pen (make-pen :fill (apply #'rgb `(,@color ,(node-intensity node)))
                                  :stroke (apply #'rgb `(,@color 1.0)))
                (circle wx wy radius)
                (mapc (lambda (child)
                        (line wx wy
                              (logical-to-window-x (node-x child))
                              (logical-to-window-y (node-y child))))
                      (node-children node)))
              (let* ((reverb (node-reverb node))
                     (radius (+ (* reverb 5) (* (- 1 reverb) radius))))
                (with-pen (make-pen :fill (apply #'rgb `(,@color 1.0)))
                  (circle (+ wx (* radius (sin (node-pan node))))
                          (- wy (* radius (cos (node-pan node))))
                          5)))))
          (node-base-nodes node-base)))
  ;; draw cursors
  (maphash
   (lambda (player-id cursor)
     (with-slots ((logical-mouse-x x)
                  (logical-mouse-y y)
                  state
                  edit-parameter
                  logical-candidate-x
                  logical-candidate-y
                  base-node
                  selected-node
                  selected-channel)
         cursor
       (with-pen (let ((color (channel-color (aref *channel-settings* selected-channel))))
                   (make-pen :fill (apply #'rgb `(,@color 0.4))
                             :stroke (apply #'rgb `(,@color 0.8))))
         (when (or (eq state :insert) (eq state :select))
           (setq base-node (find-nearest-node node-base logical-mouse-x logical-mouse-y)))
         (multiple-value-bind (cx cy)
             (get-candidate-coordinate (node-x base-node)
                                       (node-y base-node)
                                       logical-mouse-x
                                       logical-mouse-y)
           (let ((wx (logical-to-window-x cx))
                 (wy (logical-to-window-y cy)))
             (text (format nil "P~S: ~S, ~S" player-id (mod cx 1) cy) wx wy)
             (case state
               (:insert
                (setq logical-candidate-x cx)
                (setq logical-candidate-y cy)
                (if (and (= cx (node-x base-node))
                         (= cy (node-y base-node)))
                    (setq selected-node base-node)
                    (progn
                      (setq selected-node nil)
                      (with-pen (let ((color (channel-color
                                              (aref *channel-settings* selected-channel))))
                                  (make-pen :fill (apply #'rgb `(,@color 1.0))
                                            :stroke (apply #'rgb `(,@color 1.0))))
                        (crosshair wx wy))
                      (with-pen (let ((color (node-color-list base-node)))
                                  (make-pen :fill (apply #'rgb `(,@color 0.4))
                                            :stroke (apply #'rgb `(,@color 0.8))))
                        (line wx wy
                              (logical-to-window-x (node-x base-node))
                              (logical-to-window-y (node-y base-node)))))))
               (:move
                (node-move-to selected-node cx cy)))))

         (case state
           (:insert
            (crosshair (logical-to-window-x logical-mouse-x) (logical-to-window-y logical-mouse-y)))
           (:select
            (circle (logical-to-window-x logical-mouse-x) (logical-to-window-y logical-mouse-y) 3))))))
   cursors))
(defun ensure-player-id (cursors player-id)
  (unless (gethash player-id cursors)
    (setf (gethash player-id cursors) (make-cursor)))
  (gethash player-id cursors))
(defun handle-mousemotion (window player-id x y yrel)
  (with-slots (cursors) window
    (with-slots ((logical-mouse-x x) (logical-mouse-y y) selected-node state edit-parameter)
        (ensure-player-id cursors player-id)
      (setq logical-mouse-x x)
      (setq logical-mouse-y y)
      (case state
        (:edit
         (let ((delta (* 4 yrel)))
           (case edit-parameter
             (pan (node-change-param selected-node edit-parameter delta (lambda (x) (- (mod (+ x pi) +two-pi+) pi))))
             (otherwise (node-change-param selected-node edit-parameter delta)))))))))
(defmethod kit.sdl2:mousemotion-event ((window tree)
                                       timestamp button-mask x y xrel yrel)
  (let ((x (window-to-logical-x x))
        (y (window-to-logical-y y))
        (yrel (- (/ yrel *window-height*))))
    (with-slots (player-id socket side-socket) window
      (handle-mousemotion window player-id x y yrel)
      (pzmq:send (if (= player-id 0)
                     socket side-socket)
                 (write-to-string (list 'handle-mousemotion player-id x y yrel))))))
(defun handle-mousebutton (window player-id new-id mouse-state button)
  (with-slots (node-base cursors)
      window
    (with-slots (base-node
                 logical-candidate-x logical-candidate-y
                 state selected-node selected-channel)
        (ensure-player-id cursors player-id)
      (case mouse-state
        (:mousebuttondown
         (case button
           (1 (case state
                (:insert
                 (if (and (= (node-x base-node) logical-candidate-x)
                          (= (node-y base-node) logical-candidate-y))
                     (when (node-parent base-node)
                       (setq state :move)
                       (setq base-node (node-parent base-node)))
                     (progn
                       (unless new-id (setq new-id (gen-id player-id)))
                       (let ((new-node
                               (make-node :x logical-candidate-x
                                          :y logical-candidate-y
                                          :parent base-node
                                          :channel selected-channel
                                          :intensity (node-intensity base-node)
                                          :decay (node-decay base-node)
                                          :pan (node-pan base-node)
                                          :reverb (node-reverb base-node)
                                          :mute (node-mute base-node)
                                          :id new-id)))
                         (node-base-add node-base new-node)))))
                (:select
                 (setq selected-node base-node)
                 (setq state :edit))))
           (3 (case state
                (:insert (node-base-delete node-base base-node))
                (:select (let ((newval (not (node-mute base-node))))
                           (labels ((process-node (node)
                                      (setf (node-mute node)
                                            newval)
                                      (mapc #'process-node (node-children node))))
                             (process-node base-node))))))))
        (:mousebuttonup
         (case state
           (:move (setq state :insert))
           (:edit
            (setq selected-node nil)
            (setq state :select)))))))
  new-id)
(defmethod kit.sdl2:mousebutton-event ((window tree) mouse-state timestamp button x y)
  (with-slots (player-id socket side-socket) window
    (let ((new-id (handle-mousebutton window player-id nil mouse-state button)))
      (pzmq:send (if (= player-id 0)
                     socket side-socket)
                 (write-to-string (list 'handle-mousebutton player-id new-id mouse-state button))))))
(defun handle-textinput (window player-id text)
  (with-slots (cursors) window
    (with-slots (selected-channel state edit-parameter) (ensure-player-id cursors player-id)
      (case (aref text 0)
        (#\1 (setq selected-channel 0))
        (#\2 (setq selected-channel 1))
        (#\i (setq state :insert))
        (#\e
         (setq edit-parameter 'intensity)
         (setq state :select))
        (#\d
         (setq edit-parameter 'decay)
         (setq state :select))
        (#\p
         (setq edit-parameter 'pan)
         (setq state :select))
        (#\r
         (setq edit-parameter 'reverb)
         (setq state :select))))))
(defmethod kit.sdl2:textinput-event ((window tree) ts text)
  (with-slots (player-id socket side-socket) window
    (handle-textinput window player-id text)
    (pzmq:send (if (= player-id 0)
                   socket side-socket)
               (write-to-string (list 'handle-textinput player-id text)))))
(defmethod initialize-instance :before ((window tree) &rest things)
  (multiple-value-bind (_ w h) (sdl2:get-current-display-mode 0)
    (setq *window-width* (round (* w 3/4)))
    (setq *window-height* (round (* h 3/4)))))
(defmethod initialize-instance :after ((window tree) &rest things &key (player-id 0))
  (setq pzmq:*default-context* (pzmq:ctx-new))
  (setf (slot-value window 'player-id) player-id)
  (sdl2:hide-cursor)
  (with-slots (socket side-socket player-id width height title) window
    (setq title (format nil "Just Tree (Player ~S)" player-id))
    (sdl2:set-window-title (kit.sdl2:sdl-window window) title)
    (if (= player-id 0)
        (progn
          (setq socket (pzmq:socket pzmq:*default-context* :pub))
          (pzmq:bind socket "tcp://*:2333")
          (setq side-socket (pzmq:socket pzmq:*default-context* :pull))
          (pzmq:bind side-socket "tcp://*:2334"))
        (progn
          (setq socket (pzmq:socket pzmq:*default-context* :sub))
          (pzmq:setsockopt socket :subscribe "")
          (pzmq:connect socket "tcp://cat-v.mit.edu:2333")
          (setq side-socket (pzmq:socket pzmq:*default-context* :push))
          (pzmq:connect side-socket "tcp://cat-v.mit.edu:2334")))))
(defmethod kit.sdl2:window-event ((window tree) type ts d1 d2)
  (with-slots (socket side-socket) window
    (case type
      (:close
       (if socket
           (pzmq:close socket)
           (format t "Warning: socket not created?~%"))
       (if side-socket
           (pzmq:close side-socket)
           (format t "Warning: side socket not created?~%"))
       (pzmq:ctx-destroy pzmq:*default-context*)
       (setq pzmq:*default-context* nil))))
  (call-next-method))
