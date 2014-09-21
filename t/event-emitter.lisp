(in-package :cl-user)
(defpackage event-emitter-test
  (:use :cl
        :event-emitter
        :cl-test-more))
(in-package :event-emitter-test)

(plan 13)

(defclass person (event-emitter)
  ((name :initarg :name
         :reader name)))

(defparameter *user*
  (make-instance 'person :name "Eitaro Fukamachi"))

(defun hi () (format t "Hi!"))
(defun hows-it-going () (format t "How's it going?"))

;; Attach a event listener for an event ':say-hi'.
(let ((res (on :say-hi *user* #'hi)))
  (is-type res 'vector)
  (is (length res) 1))

;; *user* says 'Hi!' when an event ':say-hi' is invoked.
(is-print (emit :say-hi *user*)
          "Hi!")

(is-print (emit :say-hi *user*)
          "Hi!")

;; Attach an one time event listener.
(let ((res (once :say-hi *user* #'hows-it-going)))
  (is-type res 'vector)
  (is (length res) 2))

(is (listener-count *user* :say-hi) 2)

;; 'Hi!' and "How's it going?" will be printed.
(is-print (emit :say-hi *user*)
          "Hi!How's it going?")
(is-print (emit :say-hi *user*)
          "Hi!")

(once :say-hi *user* #'hows-it-going)

(is (listener-count *user* :say-hi) 2)

(remove-listener *user* :say-hi (lambda () "not attached listener"))
(is (listener-count *user* :say-hi) 2
    "Ignore not attached listener")

(remove-listener *user* :say-hi #'hows-it-going)
(is (listener-count *user* :say-hi) 1
    "Can remove an once event listener")

(remove-listener *user* :say-hi #'hi)
(is (listener-count *user* :say-hi) 0
    "Can remove an event listener")

(finalize)
