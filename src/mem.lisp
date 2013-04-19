(in-package :famiclom)

(defun 6502:get-byte (addr)
  (cond ((< addr #x2000) (get-byte-ram% addr))
        ((< addr #x4000) (get-byte-ppu% addr))
        ((= addr #x4016) (get-byte-input% addr))
        ((< addr #x8000) (format t "Cartridge RAM not yet implemented"))
        (t (get-mapper (nes-mapper *nes*) addr))))

(defun (setf 6502:get-byte) (new-val addr)
  (cond ((< addr #x2000) (setf (get-byte-ram% addr) new-val))
        ((< addr #x4000) (setf (get-byte-ppu% addr) new-val))
        ((< addr #x4020) (setf (get-byte-apu% addr) new-val))
        ((< addr #x8000) (format t "Cartridge RAM not yet implemented"))
        (t (set-mapper (nes-mapper *nes*) addr new-val))))

(defun 6502:get-range (start end)
  (coerce (loop for i from start to (1- end)
             collecting (6502:get-byte i)) 'vector))

(defun wrap-ram (addr)
  "Wrap ADDR to index the CPU's RAM."
  (logand addr #x7ff))

(defun wrap-pattern-table (addr)
  "Wrap ADDR to index into a PPU pattern table."
  (logand addr #x0fff))

(defun wrap-nametable (addr)
  "Wrap ADDR to index into a PPU name table."
  (logand addr #x03ff))

(defun wrap-palette (addr)
  "Wrap ADDR to index into the PPU palette."
  (logand addr #x1f))

(defun get-byte-ram% (addr)
  (aref (nes-ram *nes*) (wrap-ram addr)))

(defun (setf get-byte-ram%) (new-val addr)
  (setf (aref (nes-ram *nes*) (wrap-ram addr)) new-val))

(defun get-page (rom n &optional (size #x1000))
  "Given a ROM and page, N, return the Nth 4kb chunk of sprite data."
  (let ((start (* #x1000 n)))
    (subseq (rom-chr rom) start (+ start size))))

(defun wrap-bank (addr)
  "Wrap an address, ADDR, to a 16k ROM bank."
  (logand addr #x3fff))

(defun high-bank-p (addr)
  "Is ADDR accessing the high ROM bank (i.e. > #xc000)."
  (logbitp 14 addr))

(defun lsb (num)
  "Gets the least significant byte of an int or bit of a byte."
  (etypecase num
    (u8 (ldb (byte 1 0) num))
    (u16 (ldb (byte 8 8) num))))

(defun msb (num)
  "Gets the most significant byte of an int or bit of a byte."
  (etypecase num
    (u8 (ldb (byte 1 7) num))
    (u16 (ldb (byte 8 8) num))))
