(in-package :famiclom)

(defstruct apu
  "The Nintendo Audio Processing Unit."
  (regs  (bytevector #x17)))

