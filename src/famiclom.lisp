(in-package :famiclom)

(defstruct nes
  "A \"headless\" NES."
  (cpu    (make-cpu))
  (ppu    (make-ppu))
  (apu    (make-apu))
  (mapper nil)
  (rom    nil))

(defvar *nes* (make-nes))

(defun load-rom (file)
  "Load the given FILE into the NES."
  (let* ((rom (romreader:load-rom file))
         (mapper (getf (rom-metadata rom) :mapper)))
    (setf (nes-rom *nes*) rom)
    (setf (nes-mapper *nes*) (make-instance (cdr mapper)))))

;; init for the nes is basically: load rom, absolute jmp to the contents of $FFFC
