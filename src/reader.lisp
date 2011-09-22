(in-package :ocilib)

(defun get-result-set (statement)
  (check-errors
   (oci-get-resultset statement)))

(defparameter *result-set* nil)

(defmacro with-result-set ((&optional
                            (var '*result-set*)
                            (statement '*statement*))
                           (&rest fields)
                           &body body)
  (flet ((accessor (type)
           (case type
             (:int 'get-int)
             (:string 'get-string)
             (t 'get-value))))
    (loop :for spec :in fields
       :for (f type) = (alexandria:ensure-list spec)
       :for name = (symbol-name f)
       :for i = (gensym name)
       :collect (list i `(get-column-index ,name ,var)) :into inits
       :collect (list f (list (accessor type) i var)) :into accessors
       :finally
       (return
         `(let* ((,var (get-result-set ,statement))
                 ,@inits)
            (symbol-macrolet ,accessors ,@body))))))

(defun fetch-first (&optional (result-set *result-set*))
  (not (zerop (oci-fetch-first result-set))))

(defun fetch-next (&optional (result-set *result-set*))
  (not (zerop (oci-fetch-next result-set))))

(defun get-column-index (name &optional (result-set *result-set*))
  (let ((index (oci-get-column-index result-set name)))
    (if (zerop index)
        (error "There is no column named `~a'" name)
        index)))

(defun get-int (index &optional (result-set *result-set*))
  (oci-get-int result-set index))

(defun get-string (index &optional (result-set *result-set*))
  (oci-get-string result-set index))

(defun get-column (name-or-index &optional (result-set *result-set*))
  (etypecase name-or-index
    (string (oci-get-column2 result-set  name-or-index))
    (number (oci-get-column result-set name-or-index))))

(defun get-accessor (index &optional (result-set *result-set*))
  (case (oci-column-get-type (get-column index result-set))
    (:string #'get-string)
    (:numeric #'get-int)
    (t #'get-string)))

(defun get-value (index &optional (result-set *result-set*))
  (funcall (get-accessor index result-set) index result-set))

(defun execute-scalar (sql &optional (connection *connection*))
  (with-statement (sql connection)
    (with-result-set () ()
      (and (ocilib:fetch-next)
           (ocilib::get-value 1)))))

(defun execute-non-query (sql &optional (connection *connection*))
  (with-statement (sql connection)))

(defun commit (&optional (connection *connection*))
  (oci-commit connection))

(defun rollback (&optional (connection *connection*))
  (oci-rollback connection))