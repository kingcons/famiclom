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

(deftype bool () '(unsigned-byte 1))

(defstruct pad
  (buttons (make-array 8 :element-type 'bool))
  (strobe :a :type keyword))

(defvar *pad* (make-pad) "An input device to retrieve commands from.")

(defmacro with-event ((&optional (var 'event)) &body body)
  "Poll until we receive an event, then execute BODY in a case on the event-type.
In BODY, EVENT is bound to the current event and KEY to a function that returns
the keypress of the event if it is of type :key-down-event."
  `(let ((,var (lispbuilder-sdl:new-event)))
     (setf lispbuilder-sdl:*sdl-event* ,var)
     (unwind-protect (loop until (zerop (sdl-cffi::sdl-poll-event ,var))
                        do (flet ((key (x) (sdl::key-key x)))
                             (case (lispbuilder-sdl:event-type ,var)
                               ,@body)))
       (lispbuilder-sdl:free-event ,var))))

(defgeneric get-state (pad)
  (:documentation "Get the current state of PAD.")
  (:method ((pad pad))
    (aref (pad-buttons pad) (%strobe-state (pad-strobe pad)))))

(defgeneric next-state (pad)
  (:documentation "Update the strobe value of PAD.")
  (:method ((pad pad))
    (with-accessors ((strobe pad-strobe)) pad
      (setf strobe (%strobe-state strobe :next)))))

(defgeneric get-input (pad)
  (:documentation "Check for input from the user.")
  (:method (pad)
    (with-event ()
        (:quit-event :quit)
        (:key-down-event (handle-input (key event) pad)))))

(defun handle-input (key pad)
  "Update the state for the given KEY."
  (case key
    (:sdl-key-escape :quit)
    (t (alexandria:when-let (index (%keymap key))
         (setf (aref (pad-buttons pad) index) 1)))))

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

