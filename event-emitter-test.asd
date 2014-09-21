(in-package :cl-user)
(defpackage event-emitter-test-asd
  (:use :cl :asdf))
(in-package :event-emitter-test-asd)

(defsystem event-emitter-test
  :depends-on (:event-emitter
               :cl-test-more)
  :components ((:file "t/event-emitter"))

  :defsystem-depends-on (:cl-test-more)
  :perform (test-op :after (op c)
                    (funcall (intern #. (string :run-test-system) :cl-test-more)
                             c)
                    (asdf:clear-system c)))
