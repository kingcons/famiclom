(in-package :famiclom)

(defgeneric init (backend)
  (:documentation "Initialize *screen* for the given BACKEND."))

(defgeneric next-frame (nes)
  (:documentation "Compute the next frame for the NES."))

(defgeneric draw (frame)
  (:documentation "Draw (i.e. blit) the given FRAME to *screen*."))

;; TODO: Seems to indicate that get-sprite, read-oam, and get-visible-sprites are first targets.
(defun optimize-test (&optional (rom "smb.nes"))
  (reset *nes*)
  (load-rom (app-path rom))
  (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)
    (setf *screen* (sdl:window 256 240 :bpp 24 :sw t))
    (with-accessors ((cpu nes-cpu)
                     (ppu nes-ppu)) *nes*
      (time
       (loop until (> (6502:cpu-cc cpu) +cycles-per-second+)
          do (let ((c-step (6502-step cpu (6502:get-byte (6502:immediate cpu))))
                   (p-step (ppu-step ppu (6502:cpu-cc cpu))))
               (when (ppu-result-vblank p-step)
                 (6502:nmi cpu))
               (when (ppu-result-new-frame p-step)
                 (sdl:update-display *screen*))))))))

(defun forget (symbol)
  (etypecase (symbol-value symbol)
    (function (fmakunbound symbol))
    (standard-generic-function (fmakunbound symbol))
    (t (makunbound symbol)))
  (unintern symbol))

