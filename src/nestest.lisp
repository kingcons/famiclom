(in-package :famiclom)

(defun init-testing ()
  ;(reset *nes*)
  (setf *nes* (make-nes))
  (load-rom (app-path "tests/nestest.nes"))
  (setf (6502:cpu-pc (nes-cpu *nes*)) #xc000))

(defmethod log-state ((cpu cpu))
  (with-accessors ((ar 6502:cpu-ar)
                   (xr 6502:cpu-xr)
                   (yr 6502:cpu-yr)
                   (sr 6502:cpu-sr)
                   (sp 6502:cpu-sp)) cpu
    (format nil "A:~2,'0x X:~2,'0x Y:~2,'0x P:~2,'0x SP:~2,'0x" ar xr yr sr sp)))

(defun find-bug ()
  (init-testing)
  ;; Py65, SprocketNES, and TENES all use different vals for the CPUs reset state.
  ;; Just appease the nestest expectations in this case.
  (with-accessors ((cpu nes-cpu)) *nes*
    (with-open-file (in (app-path "tests/nestest.log"))
      (loop for line = (read-line in nil) while line
         if (search (log-state cpu) line)
         do (6502:6502-step cpu (get-byte (6502:immediate cpu)))
         else return (format t "BUG: Got ~A, expected ~%~A~%" (log-state cpu) line)))))

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
