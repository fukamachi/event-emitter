(in-package :cl-user)
(defpackage event-emitter-test-asd
  (:use :cl :asdf))
(in-package :event-emitter-test-asd)

(defsystem event-emitter-test
  :depends-on (:event-emitter
               :prove)
  :components ((:test-file "t/event-emitter"))

  :defsystem-depends-on (:prove)
  :perform (test-op :after (op c)
                    (declare (ignore op))
                    (funcall (intern #.(string :run-test-system) :prove) c)))
