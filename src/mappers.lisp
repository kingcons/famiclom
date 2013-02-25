(in-package :famiclom)

;; Mappers 0-4 cover ~74% of all existing NES titles.
;; Support them fully. NROM, MMC1, UNROM, CNROM, MMC3
;; Bigtime TENES methods: init, shutdown, read, write.
;; Also has scanline_start, scanline_end, save_state, restore_state, ex_write, ex_read.

(defclass mapper () ((rom :initarg :rom :accessor mapper-rom)))

(defgeneric make-mapper (id &rest args)
  (:documentation "Return a mapper instance for the given ID."))

(defgeneric get-mapper (mapper address)
  (:documentation "Get the value of ADDRESS from MAPPER."))

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
             (with-accessors ((rom mapper-rom)) instance
               ,(getf schema :init)))))
     (defmethod get-mapper ((mapper ,name) address)
       (with-accessors ((rom mapper-rom)) mapper
         ,(getf schema :getter)))
     (defmethod set-mapper ((mapper ,name) address value)
       (with-accessors ((rom mapper-rom)) mapper
         ,(getf schema :setter)))))

(defmapper nrom (:id 0)
  (:init (when (plusp (getf (rom-metadata rom) :chr-size))
           (setf (ppu-pattern-table (nes-ppu *nes*)) (rom-chr rom)))
   :getter (let ((size (getf (rom-metadata rom) :prg-size)))
             (aref (rom-prg rom) (logand address (1- size))))
   :setter nil))

(defmapper mmc1 (:id 1 :slots ((regs :initform #(#x0c #x00 #x00 #x00)
                                     :accessor mapper-regs)
                               (accum :initform 0 :accessor mapper-accum)
                               (count :initform 0 :accessor mapper-count)
                               (bank :initform 0 :accessor mapper-bank)
                               (mask :initform 0 :accessor mapper-mask)))
  (:init (let ((meta (rom-metadata rom)))
           (when (>= (getf (rom-metadata rom) :prg-roms) 32)
             (setf (mapper-mask instance) 1))
           (when (plusp (getf (rom-metadata rom) :chr-size))
             (setf (ppu-pattern-table (nes-ppu *nes*)) (rom-chr rom))))
   :getter (let ((offset (logand address #x3fff)))
             'do-it) ; TODO
   :setter nil)) ; TODO

(defmethod mirroring ((mapper mmc1))
  (let ((ctrl-reg (aref (mapper-regs mapper) 0)))
    (ecase (logand ctrl-reg 3)
      (0 :lower)
      (1 :upper)
      (2 :vertical)
      (3 :horizontal))))

(defmethod prg-mode ((mapper mmc1))
  (let ((ctrl-reg (aref (mapper-regs mapper) 0)))
    (ecase (logand (ash ctrl-reg -2) 3)
      ((0 1) :switch32k)
      (2 :fix-first)
      (3 :fix-last))))

(defmethod chr-mode ((mapper mmc1))
  (let ((ctrl-reg (aref (mapper-regs mapper) 0)))
    (if (zerop (logand (ash ctrl-reg -4)))
        :switch-8k
        :switch-4k)))

(defmapper unrom (:id 2)
  (:getter nil :setter nil))
(defmapper cnrom (:id 3)
  (:getter nil :setter nil))
(defmapper mmc3 (:id 4)
  (:getter nil :setter nil))
