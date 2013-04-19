(defsystem #:famiclom
  :name "famiclom"
  :description "A headless emulator for the NES"
  :version "0.0.9"
  :license "BSD"
  :author "Brit Butler <redline6561@gmail.com>"
  :pathname "src/"
  :depends-on (:cl-6502 :romreader :lispbuilder-sdl)
  :serial t
  :components ((:file "packages")
               (:file "mappers")
               (:file "mem")
               (:file "input")
               (:file "apu")
               (:file "new-ppu")
               (:file "famiclom"))
  :in-order-to ((test-op (load-op famiclom-tests)))
  :perform (test-op :after (op c)
                    (funcall (intern "RUN!" :famiclom-tests)
                             (intern "FAMICLOM-TESTS" :famiclom-tests))))

(defsystem #:famiclom-tests
  :depends-on (:famiclom :fiveam)
  :pathname "tests/"
  :serial t
  :components ((:file "tests")))

(defmethod operation-done-p ((op test-op)
                             (c (eql (find-system :famiclom))))
  (values nil))

(defpackage #:famiclom-conf (:export #:app-path))
(defvar famiclom-conf::*basedir*
  (make-pathname :defaults *load-truename* :name nil :type nil))
(defun famiclom-conf:app-path (path &rest args)
  (merge-pathnames (apply 'format nil path args) famiclom-conf::*basedir*))
