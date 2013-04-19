(in-package :famiclom)

;; This is really just a playground to get a feel for the data layout,
;; write simple name table or pattern table viewers, etc.

(defvar *framebuffer* (bytevector (* 256 240 4))
  "A framebuffer of bytes to render out. 256x240xRGBA")

(defun ppu-restore (file)
  "Set the PPU state based on a PPU dump file from FCEUX."
  (with-accessors ((ppu nes-ppu)) *nes*
    (let ((image (alexandria:read-file-into-byte-vector
                  (app-path "roms/~a.bin" file))))
      (setf (ppu-pattern-lo ppu) (subseq image #x0000 #x1000)
            (ppu-pattern-hi ppu) (subseq image #x1000 #x2000)
            (ppu-nametable-lo ppu) (subseq image #x2000 #x2400)
            (ppu-palette ppu) (subseq image #x3f00 #x3f20)))))

(defun get-tile-bytes (n &optional (table 'ppu-pattern-hi))
  "Get the bytes comprising the Nth tile in the pattern-table."
  (let ((offset (* #x10 n))
        (pattern-table (funcall table (nes-ppu *nes*))))
    (subseq pattern-table offset (+ offset #x10))))

(defun get-tile-addr (row col &optional (table 'ppu-nametable-lo))
  "Look up the Pattern Table address of the Name Table entry at row, col."
  (let ((offset (+ (* row 32) col))
        (nametable (funcall table (nes-ppu *nes*))))
    (aref nametable offset)))

(defun draw-bg ()
  ;; May want some with-scanline/with-tile control-flow macros here later...
  (dotimes (row 30)
    (dotimes (col 32)
      (let* ((tile-addr (get-tile-addr row col))
             (tile (make-tile :bytes (get-tile-bytes tile-addr))))
        ;; TODO: now what?
        ;; 1. determine 'attribute arena' for the tile
        ;; 2. determine palette for the attribute arena
        ;; * can we draw in 32x32 pixel blocks? render-by-attribute-arenas?
        ;; 3. render the tile to the framebuffer.
        ;; 4. after all the tiles are done, draw the framebuffer.
        ))))
