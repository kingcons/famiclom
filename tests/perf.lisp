(in-package :famiclom)

(require 'sb-sprof)

(defun profile! (&optional (mode :time))
  (reset *nes*)
  (load-rom "/home/redline/quicklisp/local-projects/famiclom/roms/smb.nes")
  (sb-sprof:with-profiling (:sample-interval 0.001
                            :alloc-interval 2
                            :max-samples 1000
                            :show-progress t
                            :report :graph
                            :mode :time
                            :reset t)
    (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)
      (sdl:window 256 240 :bpp 24 :sw t)
      (sdl:enable-key-repeat 10 10)
      (with-accessors ((cpu nes-cpu)
                       (ppu nes-ppu)) *nes*
        (loop until (> (6502:cpu-cc cpu) (* 2 (expt 2 20)))
           do (let ((c-step (step-cpu cpu (get-byte (6502:cpu-pc cpu))))
                    (p-step (ppu-step ppu (6502:cpu-cc cpu))))
                (when (ppu-result-vblank p-step)
                  (6502:nmi cpu))
                (when (ppu-result-new-frame p-step)
                  (sdl:update-display))))))))
