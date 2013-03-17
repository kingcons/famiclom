(in-package :famiclom)

;; Mappers 0-4 cover ~74% of all existing NES titles.
;; Support them fully. NROM, MMC1, UNROM, CNROM, MMC3
;; Bigtime TENES methods: init, shutdown, read, write.
;; Also has scanline_start, scanline_end, save_state, restore_state.

;; TODO: Add something like this to romreader?
;; At least add a :{prg,chr}-bank-size to rom-metadata.
(defun wrap-bank (addr)
  (logand addr #x3fff))

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
                               (prg1-reg :initform #x00 :accessor mapper-prg1)
                               (prg-ram :initform (bytevector #x2000)
                                        :accessor mapper-prg-ram)
                               (chr-ram :initform (bytevector #x2000)
                                        :accessor mapper-chr-ram)
                               (accum :initform 0 :accessor mapper-accum)
                               (writes :initform 0 :accessor mapper-writes)))
  (:init (let ((meta (rom-metadata rom)))
           (when (plusp (getf (rom-metadata rom) :chr-size))
             (setf (ppu-pattern-table (nes-ppu *nes*))
                   (subseq (rom-chr rom) 0 #x2000))))
   :getter (flet ((offset (x) (logior (* x #x4000) (wrap-bank address))))
             (aref (rom-prg rom) (offset (get-bank mapper address))))
   :setter (if (check-reset mapper value)
               nil
               (counted-write mapper address value))))

(defmethod counted-write ((mapper mmc1) addr val)
  (with-accessors ((accum mapper-accum)
                   (writes mapper-writes)) mapper
    (setf accum (logior accum (ash (logand val 1) writes)))
    (incf writes)
    (when (= writes 5)
      (cond ((< addr #x9fff) (setf (mapper-ctrl mapper) accum))
            ((< addr #xbfff) (setf (mapper-chr1 mapper) accum))
            ((< addr #xdfff) (setf (mapper-chr2 mapper) accum))
            (t (setf (mapper-prg1 mapper) accum)))
      (setf writes 0 accum 0))))

(defmethod check-reset ((mapper mmc1) val)
  (with-accessors ((accum mapper-accum)
                   (writes mapper-writes)
                   (ctrl-reg mapper-ctrl)) mapper
    (when (logbitp 7 val)
      (setf writes 0 accum 0 ctrl-reg (logior ctrl-reg #x0c)))))

(defmethod get-bank ((mapper mmc1) addr)
  (with-accessors ((prg1 mapper-prg1)
                   (rom  mapper-rom)) mapper
    (if (< addr #xc000)
        (ecase (prg-mode mapper)
          (:switch32k (logand prg1 #xfe))
          (:fix-first 0)
          (:fix-last prg1))
        (ecase (prg-mode mapper)
          (:switch32k (logior (logand prg1 #xfe) 1))
          (:fix-first prg1)
          (:fix-last (1- (getf (rom-metadata rom) :prg-roms)))))))

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
  (if (zerop (logand (ash (mapper-ctrl mapper) -4) 1))
      :switch-8k
      :switch-4k))

(defmapper unrom (:id 2)
  (:getter nil :setter nil))
(defmapper cnrom (:id 3)
  (:getter nil :setter nil))
(defmapper mmc3 (:id 4)
  (:getter nil :setter nil))
