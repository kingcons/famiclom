(in-package :famiclom)

;;;; TODO: Understand the magic of PPUs. A nightmare of state.

(defvar *resolution* '(:width 256 :height 240) "NES output resolution.")
(defvar *frame* (make-array 184320 :element-type 'u8) "A single frame to blit.")

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

;;;; Sprites: How do they work?

(declaim (inline make-sprite))
(defstruct sprite
  (x           0 :type u8)
  (y           0 :type u8)
  (tile-index  0 :type u8)
  (attribute   0 :type u8))

(defmethod palette ((sprite sprite)) (+ 4 (logand (sprite-attribute sprite) 3)))
(defmethod flip-h ((sprite sprite)) (plusp (logand (sprite-attribute sprite) #x40)))
(defmethod flip-v ((sprite sprite)) (plusp (logand (sprite-attribute sprite) #x80)))

(defmethod priority ((sprite sprite))
  (if (zerop (logand (sprite-attribute sprite) #x20))
      :above
      :below))

(defmethod on-scanline ((sprite sprite) ppu y)
  (if (< y (sprite-y sprite))
      nil
      (ecase (sprite-size ppu)
        (:8 (< y (+ (sprite-y sprite) 8)))
        (:16 (< y (+ (sprite-y sprite) 16))))))

(defmethod in-bounding-box ((sprite sprite) ppu x y)
  (and (>= x (sprite-x sprite))
       (< x (+ (sprite-x sprite) 8))
       (on-scanline sprite ppu y)))

(defmethod tiles ((sprite sprite) ppu)
  (let ((base (pattern-table-addr ppu)))
    (ecase (sprite-size ppu)
      (:8 (logior (tile-index sprite) base))
      (:16 (let* ((initial (sprite-tile-index sprite))
                  (first (logandc2 initial 1)))
             (when (plusp (logand initial 1))
               (incf first #x1000))
             (list first (1+ first)))))))

(defun from-oam (oam index)
  (make-sprite :x          (+ 3 (* 4 (aref oam index)))
               :y          (+ 0 (* 4 (aref oam index)))
               :tile-index (+ 1 (* 4 (aref oam index)))
               :attribute  (+ 2 (* 4 (aref oam index)))))

(defun get-visible-sprites (ppu)
  ; TODO: Did I mistranslate semantics here?
  (with-accessors ((oam ppu-oam)) ppu
    (loop with count = 0 with result = (make-array 8 :initial-element nil)
       for i from 0 to 64 for sprite = (from-oam oam i)
       when (on-scanline sprite ppu (getf (ppu-meta ppu) :scanline))
       do (if (< count 8)
              (setf (aref result count) i
                    count (1+ count))
              (set-sprite-overflow ppu 1))
       finally (return result))))

;;;; Graphics Cards: How do they work?

(defstruct ppu
  "The Nintendo Picture Processing Unit."
  ; (vram      (make-array #x4000 :element-type 'u8)) ; Just an abstraction. Doesn't really exist.
  (nametable (make-array #x0800 :element-type 'u8))
  (palette   (make-array #x0020 :element-type 'u8))
  (oam       (make-array #x0100 :element-type 'u8)) ; Sprite RAM/Object Attrib Mem
  (ctrl      0 :type u8)
  (mask      0 :type u8)
  (status    0 :type u8)
  (oam-addr  0 :type u8)
  (cycles    0 :type u8)
  (scroll    '(:x 0 :y 0 :next :x))
  (addr      '(:val 0 :next :hi))
  (meta      '(:scanline 0 :buffer 0 :x 0 :y 0)))

(defmethod initialize-instance :after ((ppu ppu) &key)
  ; TODO: handle variable size nametables in vram, stuff in oam, based on mapper.
  ; Does this need to be done in the init method of the mapper? Probably.
  )

(defmacro defctrl (name compare then else)
  "Define PPU control register methods." ; TODO: elaborate
  `(defmethod ,name ((ppu ppu))
     (if (zerop (logand (ppu-ctrl ppu) ,compare))
         ,then ,else)))

(defctrl x-scroll-offset    #x01 0 256)
(defctrl y-scroll-offset    #x02 0 240)
(defctrl incf-vram-addr     #x04 1 32) ; probably rename to vram-step
(defctrl sprite-table-addr  #x08 0 #x1000)
(defctrl pattern-table-addr #x10 0 #x1000)
(defctrl sprite-size        #x20 :8 :16)
(defctrl vblank-nmi         #x80 nil t)

(defmacro defmask (name compare)
  "Define PPU mask register methods." ; TODO: elaborate
  `(defmethod ,name ((ppu ppu)) (plusp (logand (ppu-mask ppu) ,compare))))

(defmask grayscale #x01)
(defmask show-bg-left #x02)
(defmask show-sprites-left #x04)
(defmask show-bg #x08)
(defmask show-sprites #x10)
(defmask strong-reds #x20)
(defmask strong-greens #x40)
(defmask strong-blues #x80)

(defmethod set-bit-n ((ppu ppu) bit val)
  (setf (ldb (byte 1 bit) (ppu-status ppu)) val))

(defun set-sprite-overflow (ppu val) (set-bit-n ppu 5 val))
(defun set-sprite-zero-hit (ppu val) (set-bit-n ppu 6 val))
(defun set-in-vblank (ppu val) (set-bit-n ppu 7 val))

(defmethod read-status ((ppu ppu))
  (setf (getf (ppu-scroll ppu) :next) :x
        (getf (ppu-addr ppu) :next) :hi)
  (ppu-status ppu))

(defmethod update-ctrl ((ppu ppu) val)
  (setf (ppu-ctrl ppu) val)
  (with-accessors ((scroll ppu-scroll)) ppu
    (let ((old-x (getf scroll :x))
          (old-y (getf scroll :y)))
      (setf (getf scroll :x) (logior (wrap-byte old-x) (x-scroll-offset ppu))
            (getf scroll :y) (logior (wrap-byte old-y) (y-scroll-offset ppu))))))

(defmethod update-scroll ((ppu ppu) val)
  (flet ((magic (old-val) ; TODO: Why? Rename after enlightenment.
           (logior (logand old-val #xff00) val)))
    (with-accessors ((scroll ppu-scroll)
                     (meta ppu-meta)) ppu
      (ecase (getf scroll :next)
        (:x (setf (getf meta :x) (magic (getf meta :x))
                  (getf scroll :x) val
                  (getf scroll :next) :y))
        (:y (setf (getf meta :y) (magic (getf meta :y))
                  (getf scroll :y) val
                  (getf scroll :next) :x))))))

(defmethod update-addr ((ppu ppu) val)
  (with-accessors ((addr ppu-addr)
                   (meta ppu-meta)) ppu
    (let ((prev (getf addr :val)))
      (ecase (getf addr :next)
        (:hi (setf (getf addr :val) (logior (logand prev #x00ff) (ash val 8))
                   (getf addr :next) :lo))
        (:lo (setf (getf addr :val) (logior (logand prev #xff00) val)
                   (getf addr :next) :hi
                   (getf meta :x)
                   ;; HACK: Fake out the X scroll register.
                   ;; TODO: Y scrolling.
                   (let* ((initial (logand (getf addr :val) #x07ff))
                          (x-base (if (< initial #x400) 0 256)))
                     (logior (wrap-byte (getf meta :x)) x-base))))))))

(defmethod read-vram ((ppu ppu) addr)
  (cond ((< addr #x2000) (aref (rom-chr (nes-rom *nes*)) addr))
        ((< addr #x3f00) (aref (ppu-nametable ppu) (logand addr #x07ff)))
        ((< addr #x4000) (aref (ppu-palette ppu) (logand addr #x1f)))
        (t (error "READ: invalid vram address ~a" addr))))

(defmethod store-vram ((ppu ppu) addr val)
  (cond ((< addr #x2000) nil)
        ((< addr #x3f00) (setf (aref (ppu-nametable ppu) (logand addr #x07ff)) val))
        ((< addr #x4000) (let ((addr (logand addr #x1f)))
                           (when (= addr #x10) (setf addr #x00))
                           (setf (aref (ppu-palette ppu) addr) val)))
        (t (error "WRITE: invalid vram address ~a" addr)))
  (incf (getf (ppu-addr ppu) :val) (incf-vram-addr ppu)))

(defmethod store-oam ((ppu ppu) val)
  (with-accessors ((addr ppu-oam-addr)) ppu
    (setf (aref (ppu-oam ppu) addr) val)
    (incf addr)))

(defun buffered-read (ppu)
  (let ((result (read-vram ppu (getf (ppu-addr ppu) :val))))
    (incf (getf (ppu-addr ppu) :val) (incf-vram-addr ppu))
    (if (< addr #x3f00)
        (prog1
            (getf (ppu-meta ppu) :buffer)
          (setf (getf (ppu-meta ppu) :buffer) result))
        result)))

;;;; Misc Helpers

(declaim (inline make-color get-color put-pixel))
(defstruct color
  (r 0 :type u8)
  (g 0 :type u8)
  (b 0 :type u8))

(defun get-color (index)
  (let ((base (* index 3)))
    (make-color :r (aref +color-palette+ (+ 2 base))
                :g (aref +color-palette+ (+ 1 base))
                :b (aref +color-palette+ (+ 0 base)))))

(defun put-pixel (x y color)
  (let ((base (+ x (* y (getf *resolution* :width)))))
    (setf (aref *frame* (+ 0 (* 3 base))) (color-r color)
          (aref *frame* (+ 1 (* 3 base))) (color-g color)
          (aref *frame* (+ 2 (* 3 base))) (color-b color))))

(defun get-pixel-color (ppu kind tile x y)
  (let ((offset (+ y (ash tile 4))))
    (case kind
      (:bg (incf offset (pattern-table-addr ppu)))
      (:sprite (incf offset (sprite-table-addr ppu))))
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
         (y (+ (getf (ppu-meta ppu) :y) (getf (ppu-meta ppu) :scanline)))
         (base (nametable-addr (round x 8) (round y 8))) ; TODO: Use floor instead?
         (tile (read-vram ppu (apply '+ (* 32 (first base)) (rest base))))
         (color (get-pixel-color ppu 'foo tile (mod x 8) (mod y 8))))
    (if (zerop color)
        nil
        (let* ((group (+ (* (round (first base) 4) 8)
                         (round (second base) 4)))
               (attrib (read-vram ppu (+ base group #x03c0)))
               (attr-color (cond ((and (< (mod (second base) 4) 2)
                                       (< (mod (first base) 4) 2)) attrib)
                                 ((< (mod (first base) 4) 2) (ash attrib -2))
                                 ((< (mod (second base) 4) 2) (ash attrib -4))
                                 (t (ash attrib -6))))
               (tile-color (logior (ash (logand attr-color #x03) 2) color))
               (palette-index (logand (read-vram ppu (+ #x3f00 tile-color)) #x3f)))
          (get-color palette-index)))))

(defun get-sprite-pixel (ppu sprites x opaque-p)
  ; TODO
  )

(defun on-top (x y)
  y ; TODO: determine sprite priority
  )

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
      (7 (store-vram ppu (getf (ppu-addr ppu) :val) new-val)))))

(defgeneric render-scanline (ppu)
  (:method ((ppu ppu)) ;; TODO: Mirroring. Scrolling?
    (let* ((bd-index (logand (read-vram ppu #x3f00) #x3f))
           (bd-color (get-color bd-index)))
      (dotimes (x (getf *resolution* :width))
        (let* ((bg-color (when (show-bg ppu)
                           (get-bg-pixel ppu x)))
               (sprite-color (when (show-sprites ppu)
                               (get-sprite-pixel x bg-color)))
               (color (cond ((and bg-color sprite-color)
                             (on-top bg-color sprite-color))
                            (bg-color bg-color)
                            (sprite-color sprite-color)
                            (t bd-color))))
          (put-pixel x (getf (ppu-meta ppu) :scanline) color))))))

(defgeneric start-vblank (ppu)
  (:method ((ppu ppu))
    (set-in-vblank ppu 1)
    (set-sprite-zero-hit ppu 0)
    (when (vblank-nmi ppu) t)))

(defgeneric new-frame (ppu)
  (:method ((ppu ppu))
    (setf (getf (ppu-meta ppu) :scanline) 0)
    (set-in-vblank ppu 0)
    t))

(defgeneric ppu-step (ppu to-cycle)
  (:method ((ppu ppu) to-cycle)
    (let ((cycles-per-scanline 124))
      (loop with result = '(:vblank-nmi nil :new-frame nil)
         for next-scanline = (+ (ppu-cycles ppu) cycles-per-scanline)
         until (> next-scanline to-cycle)
         do (progn
              (when (< (getf (ppu-meta ppu) :scanline) (getf *resolution* :height))
                (render-scanline ppu))
              (incf (getf (ppu-meta ppu) :scanline))
              (case (getf (ppu-meta ppu) :scanline)
                (241 (setf (getf result :vblank-nmi) (start-vblank ppu)))
                (261 (setf (getf result :new-frame) (new-frame ppu))))
              (incf (ppu-cycles ppu) cycles-per-scanline))
         finally (return result)))))
