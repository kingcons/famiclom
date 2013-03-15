(in-package :famiclom)

(defconstant +width+ 256)
(defconstant +height+ 240)

(defvar *frame* (bytevector 184320)
  "A single frame to blit. 256*240*3 or width*height*rgb.")

(defvar *color-palette*
  #(#x7C #x7C #x7C #x00 #x00 #xFC #x00 #x00 #xBC #x44 #x28 #xBC #x94 #x00 #x84 #xA8
    #x00 #x20 #xA8 #x10 #x00 #x88 #x14 #x00 #x50 #x30 #x00 #x00 #x78 #x00 #x00 #x68
    #x00 #x00 #x58 #x00 #x00 #x40 #x58 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #xBC #xBC #xBC #x00 #x78 #xF8 #x00 #x58 #xF8 #x68 #x44 #xFC #xD8 #x00 #xCC #xE4
    #x00 #x58 #xF8 #x38 #x00 #xE4 #x5C #x10 #xAC #x7C #x00 #x00 #xB8 #x00 #x00 #xA8
    #x00 #x00 #xA8 #x44 #x00 #x88 #x88 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #xF8 #xF8 #xF8 #x3C #xBC #xFC #x68 #x88 #xFC #x98 #x78 #xF8 #xF8 #x78 #xF8 #xF8
    #x58 #x98 #xF8 #x78 #x58 #xFC #xA0 #x44 #xF8 #xB8 #x00 #xB8 #xF8 #x18 #x58 #xD8
    #x54 #x58 #xF8 #x98 #x00 #xE8 #xD8 #x78 #x78 #x78 #x00 #x00 #x00 #x00 #x00 #x00
    #xFC #xFC #xFC #xA4 #xE4 #xFC #xB8 #xB8 #xF8 #xD8 #xB8 #xF8 #xF8 #xB8 #xF8 #xF8
    #xA4 #xC0 #xF0 #xD0 #xB0 #xFC #xE0 #xA8 #xF8 #xD8 #x78 #xD8 #xF8 #x78 #xB8 #xF8
    #xB8 #xB8 #xF8 #xD8 #x00 #xFC #xFC #xF8 #xD8 #xF8 #x00 #x00 #x00 #x00 #x00 #x00)
  "The color palette used by the graphics card.")

;;;; Graphics Cards: How do they work?

(defstruct ppu
  "The Nintendo Picture Processing Unit."
  (pattern-table (bytevector #x2000))
  (nametable     (bytevector #x0800))
  (palette       (bytevector #x0020))
  (oam           (bytevector #x0100)) ; Sprite RAM
  (ctrl          0   :type u8)
  (mask          0   :type u8)
  (status        0   :type u8)
  (oam-addr      0   :type u8)
  (scroll-x      0   :type u8)
  (scroll-y      0   :type u8)
  (scroll-next   :x  :type keyword)
  (addr          0   :type u16)
  (addr-next     :hi :type keyword)
  (buffer        0   :type u8)
  (scanline      0   :type u16)
  (cycles        0   :type fixnum)
  (meta          '(:x 0 :y 0)))

;;;; PPU Register methods

(defmacro defctrl (name compare then else)
  "Define PPU control register methods." ; TODO: elaborate
  `(defmethod ,name ((ppu ppu))
     (if (zerop (logand (ppu-ctrl ppu) ,compare))
         ,then ,else)))

(defctrl x-scroll-offset      #x01  0  256)
(defctrl y-scroll-offset      #x02  0  240)
(defctrl vram-step            #x04  1  32)
(defctrl sprite-pattern-addr  #x08  0  #x1000)
(defctrl bg-pattern-addr      #x10  0  #x1000)
(defctrl sprite-size          #x20  8  16)
(defctrl vblank-nmi           #x80 nil t)

(defmacro defmask (name compare)
  "Define PPU mask register methods." ; TODO: elaborate
  `(defmethod ,name ((ppu ppu))
     (not (zerop (logand (ppu-mask ppu) ,compare)))))

(defmask grayscale          #x01)
(defmask show-bg-left       #x02)
(defmask show-sprites-left  #x04)
(defmask show-bg            #x08)
(defmask show-sprites       #x10)
(defmask strong-reds        #x20)
(defmask strong-greens      #x40)
(defmask strong-blues       #x80)

(defmacro defstatus (name bit)
  "Define PPU status register methods." ; TODO: elaborate
  `(defmethod ,name ((ppu ppu) val)
     (setf (ldb (byte 1 ,bit) (ppu-status ppu)) val)))

(defstatus set-sprite-overflow 5)
(defstatus set-sprite-zero-hit 6)
(defstatus set-in-vblank       7)

;;;; Helpers

(defun wrap-nametable (val)
  "Wrap VAL to index into the PPU nametable."
  (logand val #x07ff))

(defun wrap-palette (val)
  "Wrap VAL to index into the PPU palette."
  (logand val #x1f))

;;;; VRAM Memory Map

(defmethod read-vram ((ppu ppu) addr)
  (cond ((< addr #x2000) (aref (ppu-pattern-table ppu) addr))
        ((< addr #x3f00) (aref (ppu-nametable ppu) (wrap-nametable addr)))
        ((< addr #x4000) (aref (ppu-palette ppu) (wrap-palette addr)))
        (t (error "READ: invalid vram address ~a" addr))))

(defmethod store-vram ((ppu ppu) addr val)
  (cond ((< addr #x2000) (setf (aref (ppu-pattern-table ppu) addr) val))
        ((< addr #x3f00) (let ((wrapped (wrap-nametable addr)))
                           (setf (aref (ppu-nametable ppu) wrapped) val)))
        ((< addr #x4000) (let ((wrapped (wrap-palette addr)))
                           (when (= wrapped #x10) (setf wrapped #x00))
                           (setf (aref (ppu-palette ppu) wrapped) val)))
        (t (error "WRITE: invalid vram address ~a" addr)))
  (incf (ppu-addr ppu) (vram-step ppu)))

;;;; Sprite RAM/Object Attribute Memory (OAM)

(defmethod read-oam ((ppu ppu) addr)
  (aref (ppu-oam ppu) addr))

(defmethod store-oam ((ppu ppu) val)
  (with-accessors ((addr ppu-oam-addr)) ppu
    (setf (aref (ppu-oam ppu) addr) val)
    (incf addr)))

(declaim (inline make-sprite))
(defstruct sprite
  (x           0 :type u8)
  (y           0 :type u8)
  (tile-index  0 :type u8)
  (attribute   0 :type u8))

(defmethod palette ((sprite sprite))
  (+ 4 (logand (sprite-attribute sprite) 3)))

(defmethod flip-h ((sprite sprite))
  (not (zerop (logand (sprite-attribute sprite) #x40))))

(defmethod flip-v ((sprite sprite))
  (not (zerop (logand (sprite-attribute sprite) #x80))))

(defmethod priority ((sprite sprite))
  (if (zerop (logand (sprite-attribute sprite) #x20))
      :above
      :below))

(defmethod on-scanline ((sprite sprite) (ppu ppu))
  (with-accessors ((scanline ppu-scanline)) ppu
    (if (< scanline (sprite-y sprite))
        nil
        (ecase (sprite-size ppu)
          (08 (< scanline (+ (sprite-y sprite) 08)))
          (16 (< scanline (+ (sprite-y sprite) 16)))))))

(defmethod in-bounding-box ((sprite sprite) (ppu ppu) x)
  ; KLUDGE: Y is assumed to be the current scanline.
  (and (>= x (sprite-x sprite))
       (< x (+ (sprite-x sprite) 8))
       (on-scanline sprite ppu)))

(defmethod tiles ((sprite sprite) (ppu ppu))
  (let ((base (sprite-pattern-addr ppu)))
    (ecase (sprite-size ppu)
      (8 (logior (sprite-tile-index sprite) base))
      (16 (let* ((initial (sprite-tile-index sprite))
                 (tile (logandc2 initial 1)))
            (unless (zerop (logand initial 1))
              (incf tile #x1000))
            (list tile (1+ tile)))))))

;;;; Main PPU Methods - *UNTESTED*

(defun from-oam (oam index)
  (make-sprite :y          (+ 0 (* 4 (aref oam index)))
               :tile-index (+ 1 (* 4 (aref oam index)))
               :attribute  (+ 2 (* 4 (aref oam index)))
               :x          (+ 3 (* 4 (aref oam index)))))

(defun get-visible-sprites (ppu)
  ; TODO: Did I mistranslate semantics here?
  (with-accessors ((oam ppu-oam)) ppu
    (loop with count = 0 with result = (make-array 8 :initial-element nil)
       for i from 0 to 64 for sprite = (from-oam oam i)
       when (on-scanline sprite ppu)
       do (if (< count 8)
              (setf (aref result count) i
                    count (1+ count))
              (set-sprite-overflow ppu 1))
       finally (return result))))

(defmethod read-status ((ppu ppu))
  (setf (ppu-scroll-next ppu) :x
        (ppu-addr-next ppu) :hi)
  (ppu-status ppu))

(defmethod update-ctrl ((ppu ppu) val)
  (setf (ppu-ctrl ppu) val)
  (with-accessors ((x ppu-scroll-x)
                   (y ppu-scroll-y)) ppu
    (let ((new-x (logior (wrap-byte x) (x-scroll-offset ppu)))
          (new-y (logior (wrap-byte y) (y-scroll-offset ppu))))
      (rotatef x new-x y new-y))))

(defmethod update-scroll ((ppu ppu) val)
  (flet ((magic (old-val) ; TODO: Why? Rename after enlightenment.
           (logior (logand old-val #xff00) val)))
    (with-accessors ((next ppu-scroll-next)
                     (x ppu-scroll-x)
                     (y ppu-scroll-y)
                     (meta ppu-meta)) ppu
      (ecase next
        (:x (setf (getf meta :x) (magic (getf meta :x))
                  x val next :y))
        (:y (setf (getf meta :y) (magic (getf meta :y))
                  y val next :x))))))

(defmethod update-addr ((ppu ppu) val)
  (with-accessors ((meta ppu-meta)
                   (addr ppu-addr)
                   (addr-next ppu-addr-next)) ppu
    (let ((prev addr))
      (ecase addr-next
        (:hi (setf addr (logior (logand prev #x00ff) (ash val 8))
                   addr-next :lo))
        (:lo (setf addr (logior (logand prev #xff00) val)
                   addr-next :hi
                   (getf meta :x)
                   ;; HACK: Fake out the X scroll register.
                   ;; TODO: Y scrolling.
                   (let* ((initial (wrap-nametable addr))
                          (x-base (if (< initial #x400) 0 256)))
                     (logior (wrap-byte (getf meta :x)) x-base))))))))

(defun buffered-read (ppu)
  (let* ((addr (ppu-addr ppu))
         (result (read-vram ppu addr)))
    (incf (ppu-addr ppu) (vram-step ppu))
    (if (< addr #x3f00)
        (prog1
            (ppu-buffer ppu)
          (setf (ppu-buffer ppu) result))
        result)))

;;;; Misc Helpers

(declaim (inline make-color get-color put-pixel))
(defstruct color
  (r 0 :type u8)
  (g 0 :type u8)
  (b 0 :type u8))

(defun get-color (index)
  (let* ((base (* index 3))
         (red (aref *color-palette* (+ 2 base)))
         (green (aref *color-palette* (+ 1 base)))
         (blue (aref *color-palette* (+ 0 base))))
    ;(sdl:color :r red :g green :b blue)
    (+ (ash red 16) (ash green 8) blue)))

(defun put-pixel (x y color)
  (let ((base (+ x (* y +width+))))
    (setf (aref *frame* (+ 0 (* 3 base))) (color-r color)
          (aref *frame* (+ 1 (* 3 base))) (color-g color)
          (aref *frame* (+ 2 (* 3 base))) (color-b color))))

(defun get-pixel-color (ppu kind tile x y)
  (let ((offset (+ y (ash tile 4))))
    (case kind
      (:bg (incf offset (bg-pattern-addr ppu)))
      (:sprite (incf offset (sprite-pattern-addr ppu))))
    (let* ((plane-0 (read-vram ppu offset))
           (plane-1 (read-vram ppu (+ offset 8)))
           (bit-0 (logand (ash plane-0 (- (mod x 8) 7)) 1))
           (bit-1 (logand (ash plane-1 (- (mod x 8) 7)) 1)))
      (logior (ash 1 bit-1) bit-0))))

(defun nametable-addr (x y)
  (let* ((x (mod x 64))
         (y (mod y 60))
         (x-index (>= x 32))
         (y-index (>= y 30))
         (base (cond ((and x-index y-index) #x2c00)
                     (x-index #x2400)
                     (y-index #x2800)
                     (t #x2000))))
    (list (mod y 30) (mod x 32) base)))

(defun get-bg-pixel (ppu x)
  (let* ((x (+ (getf (ppu-meta ppu) :x) x))
         (y (+ (getf (ppu-meta ppu) :y) (ppu-scanline ppu)))
         (base (nametable-addr (round x 8) (round y 8))) ; TODO: Use floor instead?
         (tile (read-vram ppu (apply '+ (* 32 (first base)) (rest base))))
         (color (get-pixel-color ppu 'foo tile (mod x 8) (mod y 8))))
    (if (zerop color)
        nil
        (let* ((group (+ (* (round (first base) 4) 8)
                         (round (second base) 4)))
               (attrib (read-vram ppu (apply #'+ #x03c0 group base)))
               (attr-color (cond ((and (< (mod (second base) 4) 2)
                                       (< (mod (first base) 4) 2)) attrib)
                                 ((< (mod (first base) 4) 2) (ash attrib -2))
                                 ((< (mod (second base) 4) 2) (ash attrib -4))
                                 (t (ash attrib -6))))
               (tile-color (logior (ash (logand attr-color #x03) 2) color))
               (palette-index (logand (read-vram ppu (+ #x3f00 tile-color)) #x3f)))
          (get-color palette-index)))))

(defun get-sprite-pixel (ppu x opaque-p)
  ;; get-sprite-pixel needs to return nil if every visible-sprite is nil,
  ;; if none of the sprites are in our bounding box or if none are opaque.
  (let ((sprites (get-visible-sprites ppu))
        (oam (ppu-oam ppu)))
    (loop for i across sprites when i
       do (let* ((sprite (from-oam oam i))
                 (tile (tiles sprite ppu))
                 (pattern-color nil))
            ; TODO: We want to continue here. Is return right?
            (unless (in-bounding-box sprite ppu x)
              (return nil))
            (etypecase tile
              (fixnum
               (let ((x (- x (sprite-x sprite)))
                     (y (- (ppu-scanline ppu) (sprite-y sprite))))
                 (when (flip-h sprite) (setf x (- 7 x)))
                 (when (flip-v sprite) (setf y (- 7 y)))
                 (setf pattern-color (get-pixel-color ppu :sprite tile x y))))
              (list
               (error "8x16 sprite rendering unimplemented!")))
            (unless (zerop pattern-color)
              (return nil))
            (when (and (zerop i) opaque-p)
              (set-sprite-zero-hit ppu 1))
            (let* ((tile-color (logior (ash (palette sprite) 2) pattern-color))
                   (palette-index (logand (+ #x3f00 tile-color) #x3f)))
              (return (get-color (read-vram ppu palette-index))))))))

(defun on-top (x y)
  (declare (ignore x)) ; TODO: determine sprite priority
  y)

;;;; Core

(defun get-byte-ppu% (addr)
  (let ((ppu (nes-ppu *nes*)))
    (ecase (logand addr 7)
      (0 (ppu-ctrl ppu))
      (1 (ppu-mask ppu))
      (2 (read-status ppu))
      (3 0)
      (4 (error "no OAM read yet"))
      (5 0)
      (6 0)
      (7 (buffered-read ppu)))))

(defun (setf get-byte-ppu%) (new-val addr)
  (let ((ppu (nes-ppu *nes*)))
    (ecase (logand addr 7)
      (0 (update-ctrl ppu new-val))
      (1 (setf (ppu-mask ppu) new-val))
      (2 nil)
      (3 (setf (ppu-oam-addr ppu) new-val))
      (4 (store-oam ppu new-val))
      (5 (update-scroll ppu new-val))
      (6 (update-addr ppu new-val))
      (7 (store-vram ppu (ppu-addr ppu) new-val)))))

(defgeneric render-scanline (ppu)
  (:method ((ppu ppu)) ;; TODO: Mirroring. Scrolling?
    (let* ((bd-index (logand (read-vram ppu #x3f00) #x3f))
           (bd-color (get-color bd-index)))
      (dotimes (x +width+)
        (let* ((bg-color (when (show-bg ppu)
                           (get-bg-pixel ppu x)))
               (sprite-color (when (show-sprites ppu)
                               (get-sprite-pixel ppu x bg-color)))
               (color (cond ((and bg-color sprite-color)
                             (on-top bg-color sprite-color))
                            (bg-color bg-color)
                            (sprite-color sprite-color)
                            (t bd-color))))
          ;(put-pixel x (ppu-scanline ppu) color)
          (sdl:with-pixel (pixels (sdl:fp *screen*))
            (sdl:write-pixel pixels x (ppu-scanline ppu) color)))))))

(defgeneric start-vblank (ppu)
  (:method ((ppu ppu))
    (set-in-vblank ppu 1)
    (set-sprite-zero-hit ppu 0)
    (when (vblank-nmi ppu) t)))

(defgeneric new-frame (ppu)
  (:method ((ppu ppu))
    (setf (ppu-scanline ppu) 0)
    (set-in-vblank ppu 0)
    t))

(defgeneric ppu-step (ppu to-cycle)
  (:method ((ppu ppu) to-cycle)
    (with-accessors ((scanline ppu-scanline)) ppu
      (let ((cycles-per-scanline 114))
        (loop with result = '(:vblank-nmi nil :new-frame nil)
           for next-scanline = (+ (ppu-cycles ppu) cycles-per-scanline)
           until (> next-scanline to-cycle)
           do (progn
                (when (< scanline +height+)
                  (render-scanline ppu))
                (incf scanline)
                (case scanline
                  (241 (setf (getf result :vblank-nmi) (start-vblank ppu)))
                  (261 (setf (getf result :new-frame) (new-frame ppu))))
                (incf (ppu-cycles ppu) cycles-per-scanline))
           finally (return result))))))
