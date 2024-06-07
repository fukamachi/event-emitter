(defsystem "event-emitter-benchmark"
  :depends-on ("event-emitter")
  :serial t
  :components ((:file "t/benchmark"))
  :perform (test-op :after (o c)
                    (uiop:symbol-call :event-emitter-benchmark :main)))
