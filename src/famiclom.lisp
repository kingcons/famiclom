(in-package :famiclom)

(defstruct nes
  "A \"headless\" NES."
  (cpu    (make-cpu))
  (ram    (make-array #x0800 :element-type 'u8))
  (ppu    (make-ppu))
  (apu    (make-apu))
  (mapper nil)
  (rom    nil))

(defmethod reset :after ((nes nes))
  (let ((ppu (nes-ppu nes)))
    (setf (ppu-scroll ppu) '(:x 0 :y 0 :next :x)
          (ppu-addr ppu) '(:val 0 :next :hi)
          (ppu-meta ppu) '(:scanline 0 :buffer 0 :x 0 :y 0))))

(defvar *nes* (make-nes))
(defparameter *debug* t)

(defun get-byte-ram% (addr)
  (aref (nes-ram *nes*) (logand addr #x7ff)))

(defun (setf get-byte-ram%) (new-val addr)
  (setf (aref (nes-ram *nes*) (logand addr #x7ff)) new-val))

(defun 6502-cpu:get-byte (addr)
  (cond ((< addr #x2000) (get-byte-ram% addr))
        ((< addr #x4000) (get-byte-ppu% addr))
        ((< addr #x4018) (get-byte-input% addr))
        (t (get-mapper (nes-mapper *nes*) addr))))

(defun (setf 6502-cpu:get-byte) (new-val addr)
  (cond ((< addr #x2000) (setf (get-byte-ram% addr) new-val))
        ((< addr #x4000) (setf (get-byte-ppu% addr) new-val))
        ((< addr #x4018) (setf (get-byte-input% addr) new-val))
        (t (set-mapper (nes-mapper *nes*) addr new-val))))

(defun 6502-cpu:get-range (start end)
  (coerce (loop for i from start to end
             collecting (6502-cpu:get-byte i)) 'vector))

(defmethod 6502-step :before ((cpu cpu) opcode)
  (when *debug*
    (6502-cpu::disasm-ins (6502::immediate cpu))))

(defun load-rom (file)
  "Load the given FILE into the NES."
  (let* ((rom (romreader:load-rom file))
         (mapper (getf (rom-metadata rom) :mapper)))
    (setf (nes-rom *nes*) rom
          (nes-mapper *nes*) (make-mapper (car mapper)))
    (reset (nes-cpu *nes*))))

(defun play-rom (file)
  (load-rom file)
  (sdl:with-init ()
    (setf *screen* (sdl:window 256 240 :double-buffer t :sw t))
    (run)))

(defun run ()
  (with-accessors ((cpu nes-cpu)
                   (ppu nes-ppu)) *nes*
    (loop do
         (let ((c-step (6502-step cpu (6502-cpu:get-byte (6502::immediate cpu))))
               (p-step (ppu-step ppu (6502::cpu-cc cpu))))
           (when (getf p-step :vblank-nmi)
             (format t "DOING THE NMI STUFF!~%")
             (6502::nmi cpu)
             (setf (getf p-step :vblank-nmi) nil))
           (when (getf p-step :new-frame)
             (sdl:update-display *screen*)
             (setf (getf p-step :new-frame) nil))))))
