(in-package :famiclom)

(defstruct apu
  "The Nintendo Audio Processing Unit."
  (pulse-1)
  (pulse-2)
  (triangle)
  (noise)
  (status))

;;; KLUDGE: The APU also currently handles input requests. Cheating!
(defun get-byte-apu% (addr)
  0)

(defun (setf get-byte-apu%) (new-val addr)
  (cond ((= addr #x4014) (format t "OAM DMA not yet implemented."))
        ((= addr #x4016) (setf (pad-strobe *pad*) :a))
        ((< addr #x4004) (update-pulse addr new-val 0))
        ((< addr #x4008) (update-pulse addr new-val 1))
        ((< addr #x400c) (update-triangle addr new-val))
        ((< addr #x4010) (update-noise addr new-val))
        ((= addr #x4015) (update-status new-val))
        (t 0)))

(defun update-pulse (addr val which))
(defun update-triangle (addr val))
(defun update-noise (addr val))
(defun update-status (val))
