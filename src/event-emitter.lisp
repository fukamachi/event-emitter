(in-package :cl-user)
(defpackage event-emitter
  (:use :cl)
  (:export :event-emitter
           :event-emitter*
           :add-listener
           :on
           :once
           :remove-listener
           :remove-all-listeners
           :listeners
           :emit
           :listener-count))
(in-package :event-emitter)

(defclass event-emitter ()
  ((silo :initform (make-hash-table :test 'eq))))

(defstruct event-emitter*
  (silo (make-hash-table :test 'eq)))

(declaim (inline silo))
(defun silo (object)
  (slot-value object 'silo))

(defun add-listener (object event listener)
  (let* ((silo (silo object))
         (listeners (gethash event silo)))
    (if listeners
        (progn (vector-push-extend listener listeners)
               listeners)
        (setf (gethash event silo)
              (make-array 1 :element-type 'function
                            :adjustable t :fill-pointer 1
                            :initial-contents (list listener))))))

(declaim (inline on))
(defun on (event object listener)
  (add-listener object event listener))

(defun once (event object listener)
  (labels ((once-listener (&rest args)
             (apply listener args)
             (remove-listener object event #'once-listener)))
    (add-listener object event #'once-listener)))

(defun remove-listener (object event listener)
  (let* ((silo (silo object))
         (listeners (gethash event silo)))
    (unless listeners
      (return-from remove-listener))

    (setf (gethash event silo)
          (delete listener listeners)))
  (values))

(defun remove-all-listeners (object &optional event)
  (if event
      (remhash event (silo object))
      (setf (slot-value object 'silo)
            (make-hash-table :test 'eq))))

(defun listeners (object event)
  (let* ((silo (silo object))
         (listeners (gethash event silo)))
    (or listeners
        (setf (gethash event silo)
              (make-array 0 :element-type 'function
                            :adjustable t :fill-pointer 1)))))

(defun emit (event object &rest args)
  (let ((listeners (listeners object event)))
    (when (zerop (length listeners))
      (return-from emit nil))

    (map nil (lambda (listener)
             (apply listener args))
         listeners)
    T))

(defun listener-count (object event)
  (length (listeners object event)))
