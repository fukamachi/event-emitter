#|
  This file is a part of Event Emitter project.
  Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage event-emitter-asd
  (:use :cl :asdf))
(in-package :event-emitter-asd)

(defsystem event-emitter
  :name "Event Emitter"
  :version "0.0.1"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :components ((:file "src/event-emitter"))
  :description "Event mechanism for Common Lisp objects")
