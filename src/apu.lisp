(in-package :famiclom)

(defstruct apu
  "The Nintendo Audio Processing Unit."
  (pulse-1)
  (pulse-2)
  (triangle)
  (noise)
  (status))

(defun get-byte-apu% (addr) 0)

(defun (setf get-byte-apu%) (new-val addr) 0)
