(defpackage :event-emitter-benchmark
  (:use :cl
        :event-emitter))
(in-package :event-emitter-benchmark)

(defclass foo (event-emitter) ())

(defvar *foo*)

(defvar *counter* 0)

(defun unit ()
  (setf *foo* (make-instance 'foo))
  (on :say-hi *foo* (lambda () (incf *counter*)))
  (emit :say-hi *foo*))

(defun benchmark ()
  (loop :repeat 10000000 :do (unit)))

(defun main ()
  (time (benchmark)))
