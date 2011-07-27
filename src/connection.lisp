(in-package :ocilib)

(defparameter *connection* nil)

(defun initialize (&optional lib-path)
  (check-errors 
   (oci-initialize (null-pointer) lib-path :oci-env-default)))

(defun cleanup ()
  (check-errors 
   (oci-cleanup)))

(defun create-connection (db user password 
                          &optional (mode :oci-session-default))
  (check-errors 
   (oci-connection-create db user password mode)))

(defun release-connection (connection)
  (check-errors
   (oci-connection-free connection)))

(defmacro with-connection ((db user password &optional (var '*connection*)) 
                           &body body)
  `(let ((,var (create-connection ,db ,user ,password)))
     (unwind-protect (progn ,@body)
       (release-connection ,var))))
