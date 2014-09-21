# Event Emitter

Event Emitter provides an event mechanism like Node.js for Common Lisp objects.

Mostly ported from [Node.js 'events' module](http://nodejs.org/api/events.html).

## Usage

```common-lisp
;; Defining Event Emitter class.
(defclass person (event-emitter)
  ((name :initarg :name
         :reader name)))

(defvar *user*
  (make-instance 'person :name "Eitaro Fukamachi"))

;; Attach a event listener for an event ':say-hi'.
(on :say-hi *user*
    (lambda () (format t "Hi!")))

;; *user* says 'Hi!' when an event ':say-hi' is invoked.
(emit :say-hi *user*)
;-> Hi!

(emit :say-hi *user*)
;-> Hi!

(emit :say-hi *user*)
;-> Hi!

;; Attach an one time event listener.
(once :say-hi *user*
      (lambda ()
        (format t "How's it going?")))

;; 'Hi!' and "How's going?" will be printed.
(emit :say-hi *user*)
;-> Hi!
;   How's it going?

;; *user* doens't say "How's it going?" anymore.
(emit :say-hi *user*)
;-> Hi!

(emit :say-hi *user*)
;-> Hi!
```

## Differences from 'event-glue'

Although there's already similar library named [event-glue](https://github.com/orthecreedence/event-glue/), it fires events globally. On the other hand, Event Emitter fires an event only to a specific object.

## API

### [Class] event-emitter

Base standard class for 'event-emitter's.

```common-lisp
(defclass person (event-emitter)
  ((name :initarg :name)))
```

### [Strucuture] event-emitter*

Base strucuture class for 'event-emitter's.

```common-lisp
(defstruct (person :include event-emitter*)
  name)
```

### \[Function] (add-listener object event listener)<br>\[Function] (on event object listener)

Adds a listener to the end of the listeners array for the specified event.

```common-lisp
(on :connection server
    (lambda (stream) ...))
```

NOTE: `add-listener` and `on` takes 'object' and 'event' the opposite order.

### \[Function] (once event object listener)

Adds a **one time** listener for the event. This listener is invoked only the next time the event is fired, after which it is removed.

```common-lisp
(once :connection server
      (lambda (stream) ...))
```

### \[Function] (remove-listener object event listener)

Removes a listener from the listener array for the specified event.

```common-lisp
(defun connection-cb (stream)
  ...)

(on :connection server #'connection-cb)
(remove-listener server :connection #'connection-cb)
```

### \[Function] (remove-all-listeners object &optional event)

Removes all listeners, or those of the specified event.

```common-lisp
(remove-all-listeners server)
(remove-all-listeners server :connection)
```

### \[Function] (listeners object event)

Returns an array of listeners for the specified event.

### \[Function] (emit event object &rest args)

Executes each of the listeners in order with the supplied arguments.

Returns `T` if the event had listeners, `NIL` otherwise.

### \[Function] (listener-count object event)

Returns the number of listeners for a given event.

## Copyright

Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 2-Clause License.
