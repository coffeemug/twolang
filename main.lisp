(defpackage :twolang/main
  (:use :cl)
  (:import-from :clingon)
  (:import-from :twolang/cmd/repl #:make-repl-command)
  (:export
   #:main))

(in-package :twolang/main)

(defun main (argv)
  (let ((app (make-driver-command)))
    (clingon:run app argv)))

(defun make-driver-command ()
  (clingon:make-command
   :name "two"
   :description "command line utilities for twolang"
   :sub-commands (list
		  (make-repl-command))
   :handler #'driver-command-handler))

(defun driver-command-handler (cmd)
  (clingon:print-usage-and-exit cmd t))


#|

Broad language principles:
1. Integrates nicely with CL ecosystem
2. Nice typesystem (strong typing, inference, lots of compile-time
   power, don't need a phd in type theory to use anything)
3. Small kernel of good principles that fit nicely together (but not
   overly obsessive here)
4. Ergonomic. Like `str.reverse()` is really nice (and the dot makes
   methods discoverable). Multimethods are cool, but for a huge class
   of operations attaching verbs to classes is a good idea.
5. Some built-in help with state management. E.g. `str.reverse()` vs
   `str.reverse!()` enforced by the compiler is *really* nice.
6. Compiler, build system, package manager and version manager should
   all be in one executable.

Maybe/idk:
- Borrow checker?
- Concurrency should be really nice, finally
- Not sure how to do error handling; probably not conditions but idk,
  maybe. Should be very straight-forward, but not idiotic like golang.
  Erlang-style let-it-crash also seems nice.
- Built-in functional datastructures like fset? Idk.

Some early implementation principles:
- Attach source info to AST nodes and report good errors right away.
  Get error reporting right early. Will save a ton of headaches later.
- Build and end-to-end test suite early
- Don't build pieces of the toolchain until you need them.

|#

