;;; patch cl-collider
(in-package #:sc)

(defugen (dwg-plucked "DWGPlucked")
    (&optional (freq 440.0) (amp 0.5) (gate 1) (pos 0.14) (c1 1) (c3 30) (inp 0) (release 0.1) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen freq amp gate pos c1 c3 inp release) mul add))))
(defugen (membrane-hexagon "MembraneHexagon")
    (&optional (excitation 0.0) (tension 0.05) (loss 0.99999) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen excitation tension loss) mul add))))
