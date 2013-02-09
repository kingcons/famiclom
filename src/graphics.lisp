(in-package :famiclom)

(defvar *screen* nil
  "A screen object to draw to. Should be 256x240.")

(defgeneric init (backend)
  (:documentation "Initialize *screen* for the given BACKEND."))

(defgeneric next-frame (nes)
  (:documentation "Compute the next frame for the NES."))

(defgeneric draw (frame)
  (:documentation "Draw (i.e. blit) the given FRAME to *screen*."))

