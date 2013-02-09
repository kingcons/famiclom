(in-package :famiclom)

(defun get-byte-input% (addr)
  (if (= addr #x4016)
      (prog1 'get-gamepad-state 'next-state)
      0))

(defun (setf get-byte-input%) (new-val addr)
  (when (= addr #x4016)
    'reset-state))

(defvar *gamepad* nil
  "An input device to retrieve commands from.")

(defgeneric next-key (device)
  (:documentation "Get the next keypress from DEVICE."))

;; Note: A RESET method is part of the gamepad interface.
;; The GF is already imported from cl-6502.

