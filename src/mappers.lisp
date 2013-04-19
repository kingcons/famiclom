(in-package :famiclom)

;; Mappers 0-4 cover ~74% of all existing NES titles.
;; Support them fully. NROM, MMC1, UNROM, CNROM, MMC3
;; Bigtime TENES methods: init, shutdown, read, write.
;; Also has scanline_start, scanline_end, save_state, restore_state.

(defclass mapper () ((rom :initarg :rom :accessor mapper-rom)))

(defgeneric pages (mapper kind)
  (:documentation "Get the number of pages in a ROM. KIND may be :PRG or :CHR.")
  (:method ((mapper mapper) kind)
    (let ((meta (rom-metadata (mapper-rom mapper))))
      (ecase kind
        (:prg (getf meta :prg-roms))
        (:chr (* 2 (getf meta :chr-roms)))))))

(defgeneric make-mapper (id &rest args)
  (:documentation "Return a mapper instance for the given ID.")
  (:method (id &rest args) (error "This mapper is not yet supported.")))

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
  (:init (when (plusp (pages instance :chr))
           (let ((lower (subseq (rom-chr rom) 0 #x1000))
                 (upper (subseq (rom-chr rom) #x1000)))
             (setf (ppu-pattern-lo (nes-ppu *nes*)) lower
                   (ppu-pattern-hi (nes-ppu *nes*)) upper)))
   :getter (let ((size (getf (rom-metadata rom) :prg-size)))
             (aref (rom-prg rom) (logand address (1- size))))
   :setter nil))

;; NOTE: We do not support the few MMC1 games with 32kb banks/512kb+ prg rom.
(defmapper mmc1 (:id 1 :slots ((ctrl-reg :initform #x0c :accessor mapper-ctrl)
                               (chr1-reg :initform #x00 :accessor mapper-chr1)
                               (chr2-reg :initform #x00 :accessor mapper-chr2)
                               (prg1-reg :initform #x00 :accessor mapper-prg1)
                               (accum :initform 0 :accessor mapper-accum)
                               (writes :initform 0 :accessor mapper-writes)))
  (:init (when (plusp (pages instance :chr))
           (let ((lower (subseq (rom-chr rom) 0 #x1000))
                 (upper (subseq (rom-chr rom) #x1000)))
             (setf (ppu-pattern-lo (nes-ppu *nes*)) lower
                   (ppu-pattern-hi (nes-ppu *nes*)) upper)))
   :getter (let ((offset (wrap-bank address)))
             (aref (rom-prg rom) (+ (get-bank mapper address) offset)))
   :setter (if (logbitp 7 value)
               (reset mapper)
               (counted-write mapper address value))))

(defmethod get-bank ((mapper mmc1) addr)
  (with-accessors ((prg1 mapper-prg1)) mapper
    ;; TODO: Is this complete and correct?
    (let ((result (if (high-bank-p addr)
                      (ecase (prg-mode mapper)
                        (:switch-32k (logand prg1 #xfe))
                        (:fix-first 0)
                        (:fix-last prg1))
                      (ecase (prg-mode mapper)
                        (:switch-32k (logior (logand prg1 #xfe) 1))
                        (:fix-first prg1)
                        (:fix-last (1- (pages mapper :prg)))))))
      (* #x4000 result))))

(defmethod reset ((mapper mmc1))
  (with-accessors ((ctrl-reg mapper-ctrl)) mapper
    (setf (mapper-accum mapper) 0
          (mapper-writes mapper) 0
          ctrl-reg (logior ctrl-reg #x0c))))

(defmethod counted-write ((mapper mmc1) addr val)
  (with-accessors ((accum mapper-accum)
                   (writes mapper-writes)) mapper
    (setf (ldb (byte 1 writes) accum) (lsb val))
    (incf writes)
    (when (= writes 5)
      (write-reg mapper (ldb (byte 2 13) addr) accum)
      (setf writes 0 accum 0))))

(defmethod write-reg ((mapper mmc1) reg val)
  (let ((page (logand val #x1f)))
    (ecase reg
      ;; Update Ctrl Register, TODO: Mirroring
      (0 (setf (mapper-ctrl mapper) val))
      ;; Set Low VRAM Bank
      (1 (setf (mapper-chr1 mapper) val)
         (if (and (eql (chr-mode mapper) :switch-8k)
                  (< page (1- (pages mapper :chr))))
             (setf (ppu-pattern-table (nes-ppu *nes*))
                   (get-page (mapper-rom mapper) page #x2000))
             (setf (subseq (ppu-pattern-table (nes-ppu *nes*)) 0 #x1000)
                   (get-page (mapper-rom mapper) page))))
      ;; Set High VRAM Bank
      (2 (setf (mapper-chr2 mapper) val)
         (when (eql (chr-mode mapper) :switch-4k)
           (setf (subseq (ppu-pattern-table (nes-ppu *nes*)) #x1000)
                 (get-page (mapper-rom mapper) page))))
      ;; Set PRG ROM Bank
      (3 (setf (mapper-prg1 mapper) (mod val (pages mapper :prg)))))))

(defmethod mirroring ((mapper mmc1))
  (case (logand (mapper-ctrl mapper) 3)
    (0 :lower)
    (1 :upper)
    (2 :vertical)
    (3 :horizontal)))

(defmethod prg-mode ((mapper mmc1))
  (case (ldb (byte 2 2) (mapper-ctrl mapper))
    ((0 1) :switch32k)
    (2 :fix-first)
    (3 :fix-last)))

(defmethod chr-mode ((mapper mmc1))
  (if (logbitp 4 (mapper-ctrl mapper))
      :switch-4k
      :switch-8k))

;; (defmapper unrom (:id 2)
;;   (:getter nil :setter nil))
;; (defmapper cnrom (:id 3)
;;   (:getter nil :setter nil))
;; (defmapper mmc3 (:id 4)
;;   (:getter nil :setter nil))
