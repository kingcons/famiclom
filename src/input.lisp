(in-package :famiclom)

(defenum strobe-state (:a :b :select :start :up :down :left :right))
(defenum keymap ((:sdl-key-a       :a)
                 (:sdl-key-b       :b)
                 (:sdl-key-space   :select)
                 (:sdl-key-return  :start)
                 (:sdl-key-up      :up)
                 (:sdl-key-down    :down)
                 (:sdl-key-left    :left)
                 (:sdl-key-right   :right)))

(deftype bool () '(member t nil))

(defstruct pad
  (buttons (make-array 8 :element-type 'bool :initial-element nil))
  (strobe :a :type keyword))

(defvar *pad* (make-pad) "An input device to retrieve commands from.")

(defgeneric get-state (pad)
  (:documentation "Get the current state of PAD.")
  (:method ((pad pad))
    (aref (pad-buttons pad) (%strobe-state (pad-strobe pad)))))

(defgeneric next-state (pad)
  (:documentation "Update the strobe value of PAD.")
  (:method ((pad pad))
    (with-accessors ((strobe pad-strobe)) pad
      (setf strobe (%strobe-state strobe :next)))))

(defgeneric check-input (pad)
  (:documentation "Check for input from the user.")
  (:method (pad)
    (sdl:with-events ()
      (:quit-event () :quit)
      (:key-down-event (:key key)
        (case key
          (:sdl-key-escape :quit)
          (t (setf (aref (pad-buttons pad) (keymap key)) t))))
      (:idle () nil))))

(defgeneric handle-input (key down)
  (:documentation "Update the KEY state based on DOWN.")
  (:method (key down) nil))

(defun get-byte-input% (addr)
  (if (= addr #x4016)
      (prog1
          (get-state *pad*)
        (next-state *pad*))
      0))

(defun (setf get-byte-input%) (new-val addr)
  ; TODO: Should this just be (reset *gamepad*)?
  (when (= addr #x4016)
    (setf (pad-strobe *pad*) :a)))

; TODO: (sdl:enable-key-repeat 10 10)?
