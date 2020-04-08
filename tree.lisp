;;;; tree.lisp

;;; patch cl-collider
(in-package #:sc)

(defugen (dwg-plucked "DWGPlucked")
    (&optional (freq 440.0) (amp 0.5) (gate 1) (pos 0.14) (c1 1) (c3 30) (inp 0) (release 0.1) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen freq amp gate pos c1 c3 inp release) mul add))))
(in-package #:tree)

;;; Setup audio
(setf *s*
      (make-external-server
       "localhost"
       :port 48800
       ;; :server-options
       ;; (make-server-options :device "ZoomAudioD")
       ))
(server-boot *s*)
;; (server-quit *s*)
(defparameter *echo-bus* (bus-audio :chanls 2))

(proxy :echo
       (let* ((sig (in.ar *echo-bus* 2))
              (sig (hpf.ar sig 100))
              (sig (freeverb2.ar (car sig) (cadr sig) :mix 1.0 :room 4)))
         (out.ar 0 sig)))

(defsynth sawsynth ((gate 1) (freq 440)
                    (amp 0.5) (pan 0) (octave 0)
                    (intensity 0.5) (decay 0.5)
                    (out 0)  (reverb 0.25)
                    (echo-out *echo-bus*))
  (let* ((cutoff (expt 100.0 intensity))
         (decay (* 0.2 (expt 100.0 decay)))
         (freq (* freq (expt 2 octave)))
         (env (env-gen.kr (env (list 1 0) (list decay) (list -2)) :gate gate :act :free))
         (fenv (env-gen.kr (perc 0.0 0.05) :level-scale (* 3.0 freq) :level-bias freq))
         (fenv (+ fenv (* (env-gen.kr (env (list 0 1) (list 0.5) (list 2)))
                          (sin-osc.kr 3 0 (* 0.02 freq)))))
         (cenv (env-gen.kr (perc 0.0 (* decay 2) 1.0 -2.0) :level-scale (* cutoff freq) :level-bias freq))
         (sig (+
               (pan2.ar (pulse.ar (* 1.01 fenv) 0.2) 0.2)
               (pan2.ar (pulse.ar (* 0.99 fenv) 0.2) -0.2)))
         (sig (rlpf.ar sig cenv))
         (sig (pan2.ar sig pan (* amp env))))
    (out.ar out (* sig (- 1 reverb)))
    (out.ar echo-out (* sig reverb))))
(defsynth plucksynth ((gate 1) (freq 440)
                      (amp 0.5) (pan 0) (octave 1)
                      (intensity 0.1) (decay 0.5)
                      (out 0) (reverb 0.6)
                      (echo-out *echo-bus*))
  (let* ((freq (* freq (expt 2 octave)))
         (freq (* freq (+ 1 (sin-osc.kr 3 0 0.002))))
         (decay (* 0.2 (expt 100.0 decay)))
         (env (env-gen.kr (env (list 1 0) (list (min 0.3 (/ decay 16))) (list -2)) :gate gate))
         (env2 (env-gen.kr (env (list 1 0) (list decay) (list -2)) :gate gate))
         (inp (* (lf-clip-noise.ar 2000 (/ amp 3)) env))
         (sig (dwg-plucked.ar freq amp gate 0.1 1 (/ 500 (expt 500.0 intensity)) inp)))
    (detect-silence.ar sig 0.001 :act :free)
    (let ((panned (pan2.ar sig pan (* amp env2))))
      (out.ar out (* panned (- 1 reverb)))
      (out.ar echo-out (* panned reverb)))))
(synth :plucksynth :freq 55 :out 0 :amp 0.1)

(defstruct channel
  (synth 'sawsynth)
  (amp 0.2)
  (pan 0.0)
  (elevation 0.0)
  (color '(0 0.5 0.9)))
(defparameter *channel-settings*
  `#(,(make-channel)
     ,(make-channel
       :synth 'plucksynth
       :color '(0.5 0.0 0.9))))
;;; UI
(defstruct node
  (x 0 :type real)
  (y 0 :type real)
  (children nil :type list)
  (parent nil)
  (channel 0)
  (intensity 0.2)
  (decay 0.2))
(defstruct node-base
  (nodes nil))
(sdl2:hide-cursor)
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
  node-base)
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
(defun node-change-param (node param delta)
  (labels ((process-node (node)
             (setf (slot-value node param)
                   (trim-value (+ delta (slot-value node param))))
             (mapc #'process-node (node-children node))))
    (process-node node)))
(defun node-delete (node-base node)
  (symbol-macrolet ((parent-place (node-children (node-parent node)))
                    (base-place (node-base-nodes node-base)))
    (setf parent-place (delete node parent-place))
    (labels ((process-node (node)
               (setf (node-parent node) nil)
               (mapc #'process-node (node-children node))))
      (process-node node))
    (setf base-place
          (delete-if-not (lambda (node)
                           (or (node-parent node)
                               (and (= (node-x node) 0)
                                    (= (node-y node) 1))))
                         base-place))))
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
            '(3 4 5 6 7))
      (values xc yc))))
(defsketch tree
    ((title "tree")
     (width *window-width*)
     (height *window-height*)
     (initial-time (get-internal-real-time))
     (tape-x 0)
     (node-base (node-base-add (make-node-base) (make-node :y 1)))
     (mouse-x 0)
     (mouse-y 0)
     (state :insert)
     (logical-candidate-x)
     (logical-candidate-y)
     (base-node)
     (selected-node)
     (selected-channel 0))
  ;; (setq *window-width* width)
  ;; (setq *window-height* height)
  (background (gray 0.9))
  (with-pen (make-pen :stroke (rgb 0.5 0.5 1.0 0.4))
    (loop for i from 1 below *logical-width*
          do (let ((wx (logical-to-window-x i)))
               (line wx 0 wx *window-height*)))
    (loop for i from 1 below *logical-height*
          do (let ((wy (logical-to-window-y i)))
               (line 0 wy *window-width* wy))))
  (let ((new-tape-x
          (* width (mod (/ (- (get-internal-real-time) initial-time) internal-time-units-per-second 12.0) 1.0))))
    (mapc (lambda (node)
            (let ((channel (aref *channel-settings* (node-channel node))))
              (synth (channel-synth
                      channel)
                     :freq (* (node-y node) 55.0)
                     :amp (channel-amp channel)
                     :pan (channel-pan channel)
                     :intensity (node-intensity node)
                     :decay (node-decay node))))
          (find-swept-nodes node-base
                            (window-to-logical-x tape-x)
                            (window-to-logical-x new-tape-x)))
    (setq tape-x new-tape-x))
  (with-pen (make-pen :stroke (rgb 0 0.5 0.9 0.4))
    (line tape-x 0 tape-x height))
  (with-pen (make-pen :stroke (gray 0.8))
    (mapc (lambda (node)
            (let ((wx (logical-to-window-x
                       (node-x node)))
                  (wy (logical-to-window-y
                       (node-y node))))
              (with-pen (let ((color (channel-color
                                      (aref *channel-settings* (node-channel node)))))
                          (make-pen :fill (apply #'rgb `(,@color ,(node-decay node)))
                                    :stroke (apply #'rgb `(,@color 1.0))))
                (circle wx wy
                        (+ 10
                           (* 20 (node-intensity node))))
                (mapc (lambda (child)
                        (line wx wy
                              (logical-to-window-x (node-x child))
                              (logical-to-window-y (node-y child))))
                      (node-children node)))))
          (node-base-nodes node-base)))
  (with-pen (let ((color (channel-color (aref *channel-settings* selected-channel))))
              (make-pen :fill (apply #'rgb `(,@color 0.4))
                        :stroke (apply #'rgb `(,@color 0.8))))
    (let ((logical-mouse-x (window-to-logical-x mouse-x))
          (logical-mouse-y (window-to-logical-y mouse-y)))
      (when (or (eq state :insert) (eq state :select))
        (setq base-node (find-nearest-node node-base logical-mouse-x logical-mouse-y)))
      (multiple-value-bind (cx cy)
          (get-candidate-coordinate (node-x base-node)
                                    (node-y base-node)
                                    logical-mouse-x
                                    logical-mouse-y)
        (let ((wx (logical-to-window-x cx))
              (wy (logical-to-window-y cy)))
          (text (format nil "~S, ~S" cx cy) wx wy)
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
                               (make-pen :fill (apply #'rgb `(,@color ,(node-decay base-node)))
                                         :stroke (apply #'rgb `(,@color 1.0))))
                     (circle wx wy (+ 10
                                      (* 20 (node-intensity base-node)))))
                   (with-pen (let ((color (channel-color
                                           (aref *channel-settings* (node-channel base-node)))))
                               (make-pen :fill (apply #'rgb `(,@color 0.4))
                                         :stroke (apply #'rgb `(,@color 0.8))))
                     (line wx wy
                           (logical-to-window-x (node-x base-node))
                           (logical-to-window-y (node-y base-node)))))))
            (:move
             (node-move-to selected-node cx cy))))))

    (case state
      (:insert
       (line mouse-x (- mouse-y 4) mouse-x (+ mouse-y 6))
       (line (- mouse-x 5) mouse-y (+ mouse-x 5) mouse-y))
      (:select
       (circle mouse-x mouse-y 3)))))
(defmethod kit.sdl2:mousemotion-event ((window tree)
                                       timestamp button-mask x y xrel yrel)
  (with-slots (mouse-x mouse-y selected-node state) window
    (setq mouse-x x)
    (setq mouse-y y)
    (case state
      (:edit
       (node-change-param selected-node 'intensity (* (- yrel) 0.01))
       (node-change-param selected-node 'decay (* xrel 0.01))))))
(defmethod kit.sdl2:mousebutton-event ((window tree) mouse-state timestamp button x y)
  (with-slots (node-base base-node
               logical-candidate-x logical-candidate-y
               state selected-node selected-channel)
      window
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
                   (let ((new-node
                           (make-node :x logical-candidate-x
                                      :y logical-candidate-y
                                      :parent base-node
                                      :channel selected-channel
                                      :intensity (node-intensity base-node)
                                      :decay (node-decay base-node))))
                     (node-base-add node-base new-node)
                     (push new-node (node-children base-node)))))
              (:select
               (setq selected-node base-node)
               (setq state :edit))))
         (3 (node-delete node-base base-node))))
      (:mousebuttonup
       (case state
         (:move (setq state :insert))
         (:edit
          (setq selected-node nil)
          (setq state :select)))))))
(defmethod kit.sdl2:textinput-event ((window tree) ts text)
  (with-slots (selected-channel state) window
    (case (aref text 0)
      (#\1 (setq selected-channel 0))
      (#\2 (setq selected-channel 1))
      (#\i (setq state :insert))
      (#\e (setq state :select)))))
