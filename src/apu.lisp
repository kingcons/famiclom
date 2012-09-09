(in-package :famiclom)

(defstruct apu
  "The Nintendo Audio Processing Unit."
  (regs  (make-array #x17 :element-type '(unsigned-byte 8))))

