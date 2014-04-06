(in-package :famiclom)

(defvar *keymap*
  '((:a       :sdl-key-z)
    (:b       :sdl-key-x)
    (:select  :sdl-key-space)
    (:start   :sdl-key-return)
    (:up      :sdl-key-up)
    (:down    :sdl-key-down)
    (:left    :sdl-key-left)
    (:right   :sdl-key-right)))

(deftype bool () '(unsigned-byte 1))

(defstruct pad
  (buttons (make-array 8 :element-type 'bool))
  (strobe '#0=(:a :b :select :start :up :down :left :right . #0#)))

(defvar *pad* (make-pad) "An input device to retrieve commands from.")

(defun %pad-index (pad)
  (let ((key (first (pad-strobe pad))))
    (position key *keymap* :key #'first)))

(defun get-state (pad)
  "Get the state of the button currently strobed by PAD."
  (aref (pad-buttons pad) (%pad-index pad)))

(defun next-state (pad)
  "Update the strobe value of PAD."
  (pop (pad-strobe pad)))

(defun reset-strobe (pad)
  "Reset PAD's strobe starting at :a."
  (with-accessors ((strobe pad-strobe)) pad
    (loop until (eql :a (first strobe)) do (pop strobe))))

(defun get-byte-input% (addr)
  (prog1 (get-state *pad*)
    (next-state *pad*)))

(defun (setf get-byte-input%) (new-val addr)
  (reset-strobe *pad*))
