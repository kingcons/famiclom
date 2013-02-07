(in-package :famiclom)

(defstruct ppu
  "The Nintendo Picture Processing Unit."
  (vram       (make-array #x4000 :element-type 'u8))
  (sprite-ram (make-array    256 :element-type 'u8)))

(defun get-byte-ppu% (addr)
  (case (logand addr 7)
    (0 :ctrl-reg)
    (1 :mask-reg)
    (2 :stat-reg)
    (3 0)
    (4 'fail)
    (5 0)
    (6 0)
    (7 'vram-load)))

(defun (setf get-byte-ppu%) (new-val addr)
  (case (logand addr 7)
    (0 :ctrl-reg)
    (1 :mask-reg)
    (2 nil)
    (3 :oam-addr)
    (4 'write-oamdata)
    (5 'update-ppuscroll)
    (6 'update-ppuaddr)
    (7 'vram-store)))
