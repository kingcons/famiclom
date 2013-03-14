(in-package :famiclom)

(defstruct nes
  "A \"headless\" NES."
  (cpu    (make-cpu))
  (ram    (bytevector #x0800))
  (ppu    (make-ppu))
  (apu    (make-apu))
  (mapper nil))

(defvar *nes* (make-nes))
(defparameter *debug* nil)

(defun get-byte-ram% (addr)
  (aref (nes-ram *nes*) (logand addr #x7ff)))

(defun (setf get-byte-ram%) (new-val addr)
  (setf (aref (nes-ram *nes*) (logand addr #x7ff)) new-val))

(defun 6502:get-byte (addr)
  (cond ((< addr #x2000) (get-byte-ram% addr))
        ((< addr #x4000) (get-byte-ppu% addr))
        ((< addr #x4018) (get-byte-input% addr))
        (t (get-mapper (nes-mapper *nes*) addr))))

(defun (setf 6502:get-byte) (new-val addr)
  (cond ((< addr #x2000) (setf (get-byte-ram% addr) new-val))
        ((< addr #x4000) (setf (get-byte-ppu% addr) new-val))
        ((< addr #x4018) (setf (get-byte-input% addr) new-val))
        (t (set-mapper (nes-mapper *nes*) addr new-val))))

(defun 6502:get-range (start end)
  (coerce (loop for i from start to (1- end)
             collecting (6502:get-byte i)) 'vector))

(defmethod 6502-step :before ((cpu cpu) opcode)
  (when *debug* (current-instruction cpu t)))

(defun load-rom (file)
  "Load the given FILE into the NES."
  (let* ((rom (romreader:load-rom file))
         (mapper (getf (rom-metadata rom) :mapper)))
    (setf (nes-mapper *nes*) (make-mapper (car mapper) :rom rom))
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
         (let ((c-step (6502-step cpu (6502:get-byte (6502:immediate cpu))))
               (p-step (ppu-step ppu (6502:cpu-cc cpu))))
           (when (getf p-step :vblank-nmi)
             (format t "DOING THE NMI STUFF!~%")
             (6502:nmi cpu)
             (setf (getf p-step :vblank-nmi) nil))
           (when (getf p-step :new-frame)
             (format t "DOING THE DRAWING STUFF!~%")
             (sdl:update-display *screen*)
             (setf (getf p-step :new-frame) nil))))))
