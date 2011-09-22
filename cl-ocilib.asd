(in-package :cl-user)

(defpackage #:cl-ocilib-system
  (:use :cl :asdf))

(in-package #:cl-ocilib-system)

(defsystem #:cl-ocilib
  :author "Dmitry Statyvka <dmitry@statyvka.org.ua>"
  :version "0.0.1"
  :depends-on (:cffi :cl-win32 :alexandria)
  :components ((:module "src" :serial t
                        :components ((:file "packages")
                                     (:file "libraries")
                                     (:file "ffi")
                                     (:file "errors")
                                     (:file "connection")
                                     (:file "statement")
                                     (:file "reader")))))
