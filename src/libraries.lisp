(in-package :ocilib)

(eval-when (:compile-toplevel :load-toplevel)
  (define-foreign-library ocilib
    (:windows "ocilibw")))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (use-foreign-library ocilib))
