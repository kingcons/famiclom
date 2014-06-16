(in-package :famiclom)

;; Savestates and Battery-Backed Cartridge RAM

(defun get-byte-sram% (addr)
  (declare (ignore addr))
  (format t "SRAM save support not yet implemented.~%")
  0)

(defun (setf get-byte-sram%) (new-val addr)
  (declare (ignore new-val addr))
  0)

(defun get-byte-erom% (addr)
  (declare (ignore addr))
  (format t "Expansion ROM support not yet implemented.~%")
  0)

(defun (setf get-byte-erom%) (new-val addr)
  (declare (ignore new-val addr))
  0)
