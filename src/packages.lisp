(defpackage :famiclom
  (:use :cl)
  (:import-from :6502 #:make-cpu
                      #:get-byte
                      #:u8)
  (:import-from :romreader #:rom-binary
                           #:rom-metadata
                           #:rom-prg
                           #:rom-chr))
