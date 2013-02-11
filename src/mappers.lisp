(in-package :famiclom)

;; Mappers 0-4 cover ~74% of all existing NES titles.
;; Support them fully. NROM, MMC1, UNROM, CNROM, MMC3
;; Or as tenes refers: none, mmc1, konami, vrom, mmc3

;; How do we handle unsupported mappers? mapper-supported-p?

(defclass mapper () ())

(defgeneric make-mapper (id &rest args)
  (:documentation "Return a mapper instance for the given ID."))

(defgeneric get-mapper (mapper address)
  (:documentation "Get the value of ADDRESS in MAPPER."))

(defgeneric set-mapper (mapper address value)
  (:documentation "Set the ADDRESS in MAPPER to VALUE."))

; TODO: Improve docs, maybe rethink approach?
(defmacro defmapper (name (&key id slots) schema)
  "Generate a mapper class and read/init/shutdown methods from a schema.
NOTE: This macro is unhygienic in its handling of schema."
  `(progn
     (defclass ,name (mapper) (,@slots))
     (defmethod make-mapper ((id (eql ,id)) &rest args)
       (apply 'make-instance ',name args))
     ,@(when (getf schema :init)
         `((defmethod initialize-instance :after ((instance ,name) &key)
             ,(getf schema :init))))
     (defmethod get-mapper ((mapper ,name) address)
       ,(getf schema :getter))
     (defmethod set-mapper ((mapper ,name) address value)
       ,(getf schema :setter))))

(defmapper nrom (:id 0)
  (:getter (let* ((rom (nes-rom *nes*))
                  (size (getf (rom-metadata rom) :prg-size)))
             (aref (rom-prg rom) (logand address (1- size))))
   :setter nil))

;; Bigtime TENES methods: init, shutdown, read, write.
;; Also has scanline_start, scanline_end, save_state, restore_state, ex_write, ex_read.
; init: load the 8k chr block into the PPU's vram, clearing vram if no CHR is present.
; read: Index into PRG block at (addr & prg-size--)
; many other methods return constants. metadata block could hold them instead

(defmapper mmc1 (:id 1 :slots ((regs :initform #(#x0c #x00 #x00 #x00)
                                     :accessor mapper-regs)
                               (accum :initform 0 :accessor mapper-accum)
                               (counter :initform 0 :accessor mapper-counter)
                               (bank :initform 0 :accessor mapper-bank)
                               (bank-mask :initform 0 :accessor mapper-bank-mask)))
  (:init (let ((meta (rom-metadata (nes-rom *nes*))))
           (when (plusp (getf meta :chr-roms))
             'copy-page-to-vram) ; TODO
           (when (member (getf meta :prg-roms) '(32 64))
             (setf (mapper-bank-mask instance) 1)))
   :getter (let ((offset (logand address #x3fff)))
             'do-it) ; TODO
   :setter nil)) ; TODO

(defmapper unrom (:id 2)
  (:getter nil :setter nil))
(defmapper cnrom (:id 3)
  (:getter nil :setter nil))
(defmapper mmc3 (:id 4)
  (:getter nil :setter nil))
