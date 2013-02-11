(in-package :famiclom)

;;;; TODO: Understand the magic of PPUs.

(defvar *resolution* '(:width 256 :height 240) "NES output resolution.")

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
  (scroll    '(:x 0 :y 0 :next :x))
  (addr      '(:val 0 :next :hi))
  (meta      '(:scanline 0 :buffer 0 :x 0 :y 0 :cy 0)))

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
(defctrl incf-vram-addr     #x04 1 32)
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
        (t (error "WRITE: invalid vram address ~a" addr))))

(defmethod store-oam ((ppu ppu) val)
  (with-accessors ((addr ppu-oam-addr)) ppu
    (setf (aref (ppu-oam ppu) addr) val)
    (incf addr)))

;;;; Misc Helpers

(defstruct color
  (r 0 :type u8)
  (g 0 :type u8)
  (b 0 :type u8))

(defun get-color (index)
  (let ((start (* index 3)))
    (make-color :r (aref +color-palette+ (+ 2 start))
                :g (aref +color-palette+ (+ 1 start))
                :b (aref +color-palette+ (+ 0 start)))))

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
      (7 'vram-load))))

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
      (7 'vram-store))))

(defgeneric ppu-step (ppu to-cycle))
