(in-package :cl-user)

(defpackage #:cl-ocilib-system
  (:use :cl :asdf))

(in-package #:cl-ocilib-system)

(defsystem #:cl-ocilib
  :author "Dmitry Statyvka <dmitry@statyvka.org.ua>"
  :version "0.0.1"
  :depends-on (:cffi :cl-win32 :alexandria)
  :components ((:module "src"
                        :components ((:file "packages")
                                     (:file "libraries"
                                            :depends-on ("packages"))
                                     (:file "ffi" 
                                            :depends-on ("libraries"
                                                         "packages"))
                                     (:file "errors"
                                            :depends-on ("ffi"
                                                         "packages"))
                                     (:file "connection"
                                            :depends-on ("ffi"
                                                         "errors"
                                                         "packages"))
                                     (:file "statement"
                                            :depends-on ("ffi"
                                                         "errors"
                                                         "packages"
                                                         "connection"))
                                     (:file "reader"
                                            :depends-on ("ffi"
                                                         "errors"
                                                         "packages"))))))
