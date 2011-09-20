(in-package :cl-user)

(defpackage :ocilib
  (:use :cl :cffi)
  (:export #:with-connection
           #:with-statement
           #:with-result-set
           
           #:execute-statement

           #:fetch-next
           #:get-int
           #:get-string
           #:get-column
           #:get-value
           #:execute-scalar
           #:initialize
           #:commit
           #:execute-non-query
           #:with-prepared-statement
           #:with-bound-vars))

