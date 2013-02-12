(in-package :famiclom)

(defstruct nes
  "A \"headless\" NES."
  (cpu    (make-cpu))
  (ram    (make-array #x0800 :element-type 'u8))
  (ppu    (make-ppu))
  (apu    (make-apu))
  (mapper nil)
  (rom    nil))

(defvar *nes* (make-nes))

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

;; TODO: What about get-range? Used in disasm and cl-6502 utils.

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

; for debug purposes
; also look into clean tracing for get-byte*/get-mapper
(defmethod 6502::6502-step :before ((cpu 6502::cpu) opcode)
  (6502-cpu::disasm-instruction (vector opcode 0 0) 0))

(defun run ()
  (with-accessors ((cpu nes-cpu)
                   (ppu nes-ppu)) *nes*
    (loop do
         (let ((c-step (6502-step cpu (6502-cpu:get-byte (6502::immediate cpu))))
               (p-step (ppu-step ppu (6502::cpu-cc cpu))))
           (when (getf p-step :vblank-nmi)
             (6502::nmi cpu))
           (when (getf p-step :new-frame)
             (sdl:draw-surface *frame* *screen*)
             (sdl:update-display *screen*))))))
