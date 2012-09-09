(in-package :famiclom)

(defstruct ppu
  "The Nintendo Picture Processing Unit."
  (vram       (make-array #x4000 :element-type '(unsigned-byte 8)))
  (sprite-ram (make-array    256 :element-type '(unsigned-byte 8))))
