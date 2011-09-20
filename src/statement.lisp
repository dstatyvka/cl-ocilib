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

(defmacro with-prepared-statement ((sql &optional (connection '*connection*) 
                                        (var '*statement*))
                          &body body)
  `(let ((,var (create-statement ,connection)))
     (unwind-protect (progn (oci-prepare ,var ,sql) ,@body)
       (release-statement ,var))))

(defun binder (cffi-type count)
  (destructuring-bind (scalar array)
      (ecase cffi-type
        (:int32 '(oci-bind-int oci-bind-array-of-ints))
        (:int64 '(oci-bind-big-int oci-bind-array-of-big-ints))
        (win32:wide-string '(oci-bind-string oci-bind-array-of-strings)))
    (if count array scalar)))

(defmacro with-bound-vars (bindings &body body)
  (loop :for (var type len) :in bindings
     :for binder = (binder type len)
     :for name = (format nil ":~a" var)
     :for var-p = (gensym (symbol-name var))
     :for len-cdr = (when len (list len))
     :collect (list* var-p type len-cdr) :into foreigns
     :collect (list* binder '*statement* name var-p len-cdr) :into do-bind
     :collect (list var `(mem-ref ,var-p ,type)) :into symbol-macrolets
     :finally
     (return 
       `(with-foreign-objects ,foreigns
          (symbol-macrolet ,symbol-macrolets 
            (when (and ,@do-bind)
              ,@body))))))

(defun execute-statement (&optional sql (statement *statement*))
  (check-errors
   (if sql
       (oci-execute-stmt statement sql)
       (oci-execute statement))))
