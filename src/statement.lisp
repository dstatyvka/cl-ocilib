(in-package :ocilib)

(defun create-statement (&optional (connection *connection*))
  (check-errors
   (oci-statement-create connection)))

(defun release-statement (statement)
  (check-errors
   (oci-Statement-free statement)))

(defparameter *statement* nil)

(defmacro with-statement ((&optional sql (connection '*connection*) 
                                     (var '*statement*))
                          &body body)
  `(let ((,var (create-statement ,connection)))
     (unwind-protect (progn 
                       ,(when sql `(oci-execute-stmt ,var ,sql))
                       ,@body)
       (release-statement ,var))))

(defun execute-statement (&optional sql (statement *statement*))
  (check-errors
   (if sql
       (oci-execute-stmt statement sql)
       (oci-execute statement))))
