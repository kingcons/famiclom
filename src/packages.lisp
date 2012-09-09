(defpackage :famiclom
  (:use :cl)
  (:import-from :6502 #:make-cpu)
  (:import-from :romreader #:rom-binary
                           #:rom-metadata))
