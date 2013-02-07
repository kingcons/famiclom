(defsystem #:famiclom
  :name "famiclom"
  :description "A headless emulator for the NES"
  :version "0.0.1"
  :license "BSD"
  :author "Brit Butler <redline6561@gmail.com>"
  :pathname "src/"
  :depends-on (:cl-6502 :romreader)
  :serial t
  :components ((:file "packages")
               (:file "mappers")
               (:file "apu")
               (:file "input")
               (:file "ppu")
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
