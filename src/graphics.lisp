(in-package :famiclom)

(defvar *screen* nil
  "A screen object to draw to. Should be 256x240.")

(defgeneric init (backend)
  (:documentation "Initialize *screen* for the given BACKEND."))

(defgeneric next-frame (nes)
  (:documentation "Compute the next frame for the NES."))

(defgeneric draw (frame)
  (:documentation "Draw (i.e. blit) the given FRAME to *screen*."))

;; TODO: Seems to indicate that get-sprite, read-oam, and get-visible-sprites are first targets.
;; Also seems to suggest that functions may be more approriate for get-pattern-color, get-attrib,
;; get-bg-pixel, get-visible-sprites, get-sprite.
(defun optimize-test ()
  (reset *nes*)
  (load-rom (app-path "smb.nes"))
  (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)
    (setf *screen* (sdl:window 256 240 :bpp 24 :sw t))
    (with-accessors ((cpu nes-cpu)
                     (ppu nes-ppu)) *nes*
      (time (loop until (> (6502:cpu-cc cpu) +cycles-per-second+)
               do (let ((c-step (6502-step cpu (6502:get-byte (6502:immediate cpu))))
                        (p-step (ppu-step ppu (6502:cpu-cc cpu))))
                    (when (getf p-step :vblank-nmi)
                      (6502:nmi cpu)
                      (setf (getf p-step :vblank-nmi) nil))
                    (when (getf p-step :new-frame)
                      (sdl:update-display *screen*)
                      (setf (getf p-step :new-frame) nil))))))))

(defun forget (symbol)
  (etypecase (symbol-value symbol)
    (function (fmakunbound symbol))
    (standard-generic-function (fmakunbound symbol))
    (t (makunbound symbol)))
  (unintern symbol))

