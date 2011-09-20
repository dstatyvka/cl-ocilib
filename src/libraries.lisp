(in-package :ocilib)



(eval-when (:compile-toplevel :load-toplevel)
  (define-foreign-library ocilib
    (:windows "ocilibw")))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (use-foreign-library ocilib)
  
  (push #+(and :win32 :x86) :ocilib32
        #+(not (and :win32 :x86)) :ocilib 
        *features*))
