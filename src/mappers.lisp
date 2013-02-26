(in-package :famiclom)

;; Mappers 0-4 cover ~74% of all existing NES titles.
;; Support them fully. NROM, MMC1, UNROM, CNROM, MMC3
;; Bigtime TENES methods: init, shutdown, read, write.
;; Also has scanline_start, scanline_end, save_state, restore_state.

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

(defmapper mmc1 (:id 1 :slots ((ctrl-reg :initform #x0c :accessor mapper-ctrl)
                               (chr1-reg :initform #x00 :accessor mapper-chr1)
                               (chr2-reg :initform #x00 :accessor mapper-chr2)
                               (prg-bank :initform #x00 :accessor mapper-prg)
                               (prg-ram :initform (bytevector #x2000)
                                        :accessor mapper-prg-ram)
                               (chr-ram :initform (bytevector #x2000)
                                        :accessor mapper-chr-ram)
                               (accum :initform 0 :accessor mapper-accum)
                               (count :initform 0 :accessor mapper-count)
                               (bank :initform 0 :accessor mapper-bank)
                               (mask :initform 0 :accessor mapper-mask)))
  (:init (let ((meta (rom-metadata rom)))
           (when (>= (getf (rom-metadata rom) :prg-roms) 32)
             (setf (mapper-mask instance) 1))
           (when (plusp (getf (rom-metadata rom) :chr-size))
             (setf (ppu-pattern-table (nes-ppu *nes*)) (rom-chr rom))))
   :getter (with-accessors ((prg mapper-prg)) mapper
             (flet ((offset (x) (logior (* x #x4000) (logand address #x3fff))))
               (if (< address #xc000)
                   (let ((bank (ecase (prg-mode mapper)
                                 (:switch32k (logand prg #xfe))
                                 (:fix-first 0)
                                 (:fix-last prg))))
                     (aref (rom-prg rom) (offset bank)))
                   (let* ((meta (rom-metadata rom))
                          (bank (ecase (prg-mode mapper)
                                  (:switch32k (logior (logand prg #xfe) 1))
                                  (:fix-first prg)
                                  (:fix-last (1- (getf meta :prg-size))))))
                     (aref (rom-prg rom) (offset bank))))))
   :setter (with-accessors ((count mapper-count)
                            (accum mapper-accum)
                            (ctrl mapper-ctrl)) mapper
             (if (logbitp 7 value)
                 (setf count 0 accum 0 ctrl (logior ctrl #x0c))
                 (progn
                   (setf accum (logior accum (ash (logand value 1) count)))
                   (incf count)
                   (when (= count 5)
                     (cond ((< address #x9fff) (setf ctrl accum))
                           ((< address #xbfff) (setf (mapper-chr1 mapper) accum))
                           ((< address #xdfff) (setf (mapper-chr2 mapper) accum))
                           (t (setf (mapper-prg mapper) accum)))
                     (setf count 0 accum 0)))))))

(defmethod mirroring ((mapper mmc1))
  (ecase (logand (mapper-ctrl mapper) 3)
    (0 :lower)
    (1 :upper)
    (2 :vertical)
    (3 :horizontal)))

(defmethod prg-mode ((mapper mmc1))
  (ecase (logand (ash (mapper-ctrl mapper) -2) 3)
    ((0 1) :switch32k)
    (2 :fix-first)
    (3 :fix-last)))

(defmethod chr-mode ((mapper mmc1))
  (if (zerop (logand (ash (mapper-ctrl mapper) -4)))
      :switch-8k
      :switch-4k))

(defmapper unrom (:id 2)
  (:getter nil :setter nil))
(defmapper cnrom (:id 3)
  (:getter nil :setter nil))
(defmapper mmc3 (:id 4)
  (:getter nil :setter nil))
