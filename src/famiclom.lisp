(in-package :famiclom)

(defstruct nes
  "A \"headless\" NES."
  (cpu    (make-cpu))
  (ram    (bytevector #x0800))
  (ppu    (make-ppu))
  (apu    (make-apu))
  (mapper nil :type (or null mapper)))

(defconstant +cycles-per-second+ (* 1.79 (expt 2 20)))
(defvar *nes* (make-nes))
(defparameter *debug* nil)

(defmethod 6502-step :before ((cpu cpu) opcode)
  (when *debug* (current-instruction cpu t)))

(defun load-rom (file)
  "Load the given FILE into the NES."
  (let* ((rom (romreader:load-rom file))
         (mapper-id (getf (rom-metadata rom) :mapper-id)))
    (setf (nes-mapper *nes*) (make-mapper mapper-id :rom rom))
    (reset (nes-cpu *nes*))))

(defun play-rom (file)
  (reset *nes*)
  (load-rom file)
  (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)
    (sdl:window 256 240 :bpp 24 :sw t)
    (sdl:enable-key-repeat 10 10)
    (run)))

(defun run ()
  (with-accessors ((cpu nes-cpu)
                   (ppu nes-ppu)) *nes*
    (loop
       (let ((c-step (6502-step cpu (6502:getter '6502:immediate nil cpu)))
             (p-step (ppu-step ppu (6502:cpu-cc cpu))))
         (when (ppu-result-vblank p-step)
           (6502:nmi cpu))
         (when (ppu-result-new-frame p-step)
           (sdl:update-display))))))
