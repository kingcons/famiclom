(in-package :famiclom)

(defun get-byte-input% (addr)
  (if (= addr #x4016)
      (prog1 'get-gamepad-state 'next-state)
      0))

(defun (setf get-byte-input%) (new-val addr)
  (when (= addr #x4016)
    'reset-state))
