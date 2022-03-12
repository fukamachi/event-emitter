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

(defstruct (listener (:constructor make-listener (function &key once)))
  function once)

(declaim (inline silo))
(defun silo (object)
  (slot-value object 'silo))

(defun %add-listener (silo event listener &aux (listeners (gethash event silo)))
  (flet ((reinit-silo (&aux (length (1+ (length listeners))))
           (setf (gethash event silo)
                 (make-array length :element-type 'listener :adjustable t
                             :fill-pointer length :initial-contents
                             (cons listener (coerce listeners 'list))))))
    (multiple-value-bind (vector failed)
        (with-simple-restart (continue "Allocate a new non-simple array")
          (if listeners (vector-push-extend listener listeners) (reinit-silo)))
      (if (not failed) vector (reinit-silo)))))

(declaim (inline add-listener))
(defun add-listener (object event listener)
  (%add-listener (silo object) event (make-listener listener)))

(declaim (inline on))
(defun on (event object listener)
  (%add-listener (silo object) event (make-listener listener)))

(declaim (inline once))
(defun once (event object listener)
  (%add-listener object event (make-listener listener :once t)))

(defun remove-listener (object event listener &key(start 0))
  (let* ((silo (silo object))
         (listeners (gethash event silo)))
    (unless listeners
      (return-from remove-listener))

    (setf (gethash event silo)
          (delete listener listeners
                  :test #'eq
                  :count 1
				  :start start
                  :key #'listener-function)))
  (values))

(defun remove-all-listeners (object &optional event)
  (if event
      (remhash event (silo object))
      (setf (slot-value object 'silo)
            (make-hash-table :test 'eq)))
  (values))

(defun listeners (object event)
  (let* ((silo (silo object))
         (listeners (gethash event silo)))
    (or listeners
        (setf (gethash event silo)
              (make-array 0 :element-type 'listener
                            :adjustable t :fill-pointer 0)))))

(defun emit (event object &rest args)
  (let* ((listeners (listeners object event))
		 (max-size (length listeners)))
    (when (zerop max-size)
      (return-from emit nil))
	(do ((indx 0 (1+ indx)))
		((>= indx max-size))
	  (apply (listener-function (elt listeners indx)) args)
	  (when (listener-once (elt listeners indx))
		(remove-listener object event (listener-function (elt listeners indx))
						 :start indx)
		(decf max-size)
		(decf indx)))
    t))

(defun listener-count (object event)
  (length (listeners object event)))
