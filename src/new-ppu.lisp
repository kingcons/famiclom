(in-package :famiclom)

;;;; REFERENCES:
;; http://badderhacksnet.ipage.com/badderhacks/index.php - The NES PPU
;; http://wiki.nesdev.com/w/index.php/PPU_nametables
;; http://wiki.nesdev.com/w/index.php/PPU_registers

(defconstant +width+ 256
  "The screen width in pixels.")
(defconstant +height+ 240
  "The screen height in pixels.")

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

(defstruct ppu
  "The Nintendo Picture Processing Unit."
  (pattern-lo   (bytevector #x1000)  :type (simple-array u8 (4096)))
  (pattern-hi   (bytevector #x1000)  :type (simple-array u8 (4096)))
  (nametable-lo (bytevector #x0400)  :type (simple-array u8 (1024)))
  (nametable-hi (bytevector #x0400)  :type (simple-array u8 (1024)))
  (nametables   (make-array 4)       :type (simple-vector 4))
  (palette      (bytevector #x0020)  :type (simple-array u8 (32)))
  (oam          (bytevector #x0100)  :type (simple-array u8 (256)))
  (ctrl         0                    :type u8)
  (mask         0                    :type u8)
  (status       0                    :type u8)
  (oam-addr     0                    :type u8)
  (scroll       0                    :type u8)
  (scroll-next  :x                   :type keyword)
  (addr         0                    :type u16) ;; #x4000, remember?
  (addr-next    :hi                  :type keyword)
  ;; todo: addr-temp?
  (buffer       0                    :type u8)
  (scanline     0                    :type (integer 0 262))
  (cycles       0                    :type fixnum))

;;; Pattern Tables
;;; Pattern Tables are 4kb each made up of 256 16-byte tiles.

(defstruct tile
  "Two 8-byte layers, each defining part of the 8x8 pixels' palette."
  (bytes (bytevector #x10) :type (simple-array u8 (16))))

(defun line-palette (bottom top)
  "Combine the BOTTOM and TOP layers to determine the palette for the line."
  (loop for bit from 0 to 7
     collect (+ (ash (ldb (byte 1 bit) top) 1)
                (ldb (byte 1 bit) bottom))))

(defun tile-palette (tile)
  "Loop over the lines in TILE computing the palette for each line."
  (let ((bytes (tile-bytes tile)))
    (loop for i from 0 to 7
       collect (line-palette (aref bytes i) (aref bytes (+ i 8))))))

;;; Name Tables
;;; Name Tables are 1kb each made up of 30 32-byte tilemaps and 256 2-bit
;;; palette assignments (64 bytes).

(defstruct tile-map
  "A row of 32 1-byte addresses to pattern-table tiles."
  (bytes (bytevector #x20) :type (simple-array u8 (32))))

(defstruct palette
  "256 2-bit palette assignments, each covering 4 tiles."
  (bytes (bytevector #x40) :type (simple-array u8 (64))))

(defun set-mirroring (ppu style)
  "Set the mirroring STYLE used by the PPU's nametables. STYLE should be
one of: :VERTICAL, :HORIZONTAL, :LOWER, or :UPPER."
  (with-accessors ((nametables ppu-nametables)
                   (nametable-lo ppu-nametable-lo)
                   (nametable-hi ppu-nametable-hi)) ppu
    (case style
      (:vertical
       (setf nametables (vector nametable-lo nametable-hi
                                nametable-lo nametable-hi)))
      (:horizontal
       (setf nametables (vector nametable-lo nametable-lo
                                nametable-hi nametable-hi)))
      (:lower
       (setf nametables (vector nametable-lo nametable-lo
                                nametable-lo nametable-lo)))
      (:upper
       (setf nametables (vector nametable-hi nametable-hi
                                nametable-hi nametable-hi))))))

;;; PPU Registers

(defmacro defctrl (name compare then else)
  "Define PPU control register methods." ; TODO: elaborate
  `(defmethod ,name ((ppu ppu))
     (if (zerop (logand (ppu-ctrl ppu) ,compare))
         ,then ,else)))

(defctrl x-scroll-offset      #x01  0  +width+)
(defctrl y-scroll-offset      #x02  0  +height+)
(defctrl vram-step            #x04  1  #x20)
(defctrl sprite-pattern-addr  #x08  0  #x1000)
(defctrl bg-pattern-addr      #x10  0  #x1000)
(defctrl sprite-size          #x20  8  #x10)
(defctrl vblank-nmi           #x80 nil t)

(defmacro defmask (name compare)
  "Define PPU mask register methods." ; TODO: elaborate
  `(defmethod ,name ((ppu ppu))
     (logtest (ppu-mask ppu) ,compare)))

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

(defmethod read-status ((ppu ppu))
  (prog1 (ppu-status ppu)
    (setf (ldb (byte 1 7) (ppu-status ppu)) 0
          (ppu-scroll-next ppu) :x
          (ppu-addr-next ppu) :hi)))

(defmethod update-scroll ((ppu ppu) val)
  (with-accessors ((scroll ppu-scroll)
                   (next ppu-scroll-next)) ppu
    (ecase next
      (:x (setf scroll val next :y))
      (:y (setf scroll val next :x)))))

(defmethod update-addr ((ppu ppu) val)
  (with-accessors ((addr ppu-addr)
                   (next ppu-addr-next)) ppu
   (ecase next
     (:hi (setf addr (logior (logand addr #x00ff) (ash val 8))
                next :lo))
     (:lo (setf addr (logior (logand addr #xff00) val)
                next :hi)))))

;;; Object Attribute Memory (OAM)

(defun read-oam (ppu)
  "Read the Sprite Metadata at the OAM-ADDR."
  (aref (ppu-oam ppu) (ppu-oam-addr ppu)))

(defun write-oam (ppu val)
  "Write VAL to OAM-ADDR and increment OAM-ADDR."
  (with-accessors ((addr ppu-oam-addr)) ppu
    (setf (aref (ppu-oam ppu) addr) val
          addr (wrap-byte (1+ addr)))))

;;; Memory Map

(defun read-vram (ppu addr)
  (cond ((< addr #x1000) (aref (ppu-pattern-lo ppu) addr))
        ((< addr #x2000) (aref (ppu-pattern-hi ppu) (wrap-pattern-table addr)))
        ((< addr #x3f00) (let* ((index (ldb (byte 2 10) addr))
                                (nametable (aref (ppu-nametables ppu) index)))
                           (aref nametable (wrap-nametable addr))))
        ((< addr #x4000) (aref (ppu-palette ppu) (wrap-palette addr)))
        (t (error "READ: invalid vram address ~a" addr))))

(defun write-vram (ppu addr val)
  (cond ((< addr #x1000) (setf (aref (ppu-pattern-lo ppu) addr) val))
        ((< addr #x2000) (let ((wrapped (wrap-pattern-table addr)))
                           (setf (aref (ppu-pattern-hi ppu) wrapped) val)))
        ((< addr #x3f00) (let* ((index (ldb (byte 2 10) addr))
                                (nametable (aref (ppu-nametables ppu) index))
                                (wrapped (wrap-nametable addr)))
                           (setf (aref nametable wrapped) val)))
        ((< addr #x4000) (let ((wrapped (wrap-palette addr)))
                           (when (= wrapped #x10)
                             (setf wrapped #x00))
                           (setf (aref (ppu-palette ppu) wrapped) val)))
        (t (error "WRITE: invalid vram address ~a" addr)))
  (incf (ppu-addr ppu) (vram-step ppu)))

(defun buffered-read (ppu)
  "Read the value stored at PPU-ADDR. If the address is a palette read,
return the value, otherwise return PPU-BUFFER and store the value there."
  (let* ((addr (ppu-addr ppu))
         (result (read-vram ppu addr)))
    (incf (ppu-addr ppu) (vram-step ppu))
    (if (< addr #x3f00)
        (prog1 (ppu-buffer ppu)
          (setf (ppu-buffer ppu) result))
        result)))

(defun get-byte-ppu% (addr)
  "Handle a read from #x2000 - #x4000, i.e. the PPU."
  (let ((ppu (nes-ppu *nes*)))
    (case (logand addr 7)
      (0 (ppu-ctrl ppu))
      (1 (ppu-mask ppu))
      (2 (read-status ppu))
      (3 0)
      (4 (read-oam ppu))
      (5 0)
      (6 0)
      (7 (buffered-read ppu)))))

(defun (setf get-byte-ppu%) (new-val addr)
  "Handle a write to #x2000 - #x4000, i.e. the PPU."
  (let ((ppu (nes-ppu *nes*)))
    (case (logand addr 7)
      (0 (setf (ppu-ctrl ppu) new-val)) ; TODO: Scrolling/update-ctrl
      (1 (setf (ppu-mask ppu) new-val))
      (2 0)
      (3 (setf (ppu-oam-addr ppu) new-val))
      (4 (write-oam ppu new-val))
      (5 (update-scroll ppu new-val)) ;; TODO: correct?
      (6 (update-addr ppu new-val)) ;; TODO: correct?
      (7 (write-vram ppu (ppu-addr ppu) new-val)))))

;;; Utils

(defun get-bg-pixel (ppu x y))
(defun get-sprite-pixel (ppu x y))

;;; Kernel

(defstruct ppu-result
  (vblank)
  (new-frame))

(defgeneric ppu-step (ppu to-cycle)
  ;; TODO: It might be nice to have an :after method for nmi and drawing.
  (:documentation "Step the PPU until the given cycle.")
  (:method (ppu to-cycle) (make-ppu-result)))
