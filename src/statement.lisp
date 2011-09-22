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

(defun make-bound-var-name (name)
  (format nil ":~a" name))

(defmacro with-bound-variables ((name (&rest args))
                                (sql &optional (connection '*connection*))
                                &body body)
  (flet ((descr (cffi-type)
           (ecase cffi-type
             (:int32 'oci-bind-int)
             (:int64 'oci-bind-big-int)
             (:string 'oci-bind-string))))
    (loop
       :with statement = (gensym "statement")
       :for (arg-var arg-type direction-spec length) :in args
       :for stringp = (eq arg-type :string)
       :for bind-name = (make-bound-var-name arg-var)
       :for bind-var = (gensym bind-name)
       :for arg-in = (case direction-spec
                       ((:in nil) arg-var)
                       (:in-out (intern (format nil "~a-IN" 
                                                (symbol-name arg-var)))))
       :for arg-out = (case direction-spec
                        ((:in-out :out) arg-var))
       :for i :from 1
       :collect bind-name :into bound-vars
       :when arg-in :collect arg-in :into lambda-list
       :when arg-out :collect arg-out :into out-vars
       :when stringp
       :collect `(when ,bind-var
                   (foreign-free ,bind-var)) 
       :into string-cleanup
       ;; :and :collect bind-var :into foreign-strings
       :and :collect `(,bind-var :short (1+ ,(cond
                                              (length length)
                                              (arg-in `(length ,arg-in))
                                              (t (error "The length of out parameter is not specified: ~a" arg-var)))))
       :into foreign-objects
       :and :append 
       `((setf ,bind-var 
               (convert-to-foreign
                ,(cond
                  (length (alexandria:with-gensyms (s)
                            `(let ((,s (make-string 
                                        ,length :initial-element #\Nul)))
                               ,(list* 
                                 'prog1 s 
                                 (when arg-in
                                   `(setf (subseq ,s 0 (length ,arg-in))
                                          ,arg-in))))))
                  (arg-in `(or ,arg-in "")))
                'oci-string))
         (,(descr arg-type) ,statement ,bind-name ,bind-var
           ,(or length 0))
         ,@(when arg-in
                 `((when (null ,arg-in)
                     (oci-bind-set-null (oci-get-bind ,statement ,i)))))) 
       :into binding
       :else :append 
       `((,(descr arg-type) ,statement ,bind-name ,bind-var)
         ,@(when arg-in
                 `((if (null ,arg-in)
                       (oci-bind-set-null (oci-get-bind ,statement ,i))
                       (setf (mem-ref ,bind-var ,arg-type) ,arg-in)))))
       :into binding
       :and :collect (list bind-var arg-type) :into foreign-objects
       :when arg-out
       :collect `(setf ,arg-out
                       (when (zerop 
                                (oci-bind-is-null (oci-get-bind ,statement ,i)))
                         ,(if stringp
                              `(convert-from-foreign ,bind-var 'oci-string)
                              `(mem-ref ,bind-var ,arg-type))))
       :into get-out-values
       :collect `(oci-bind-set-direction (oci-get-bind ,statement ,i) 
                                         direction-spec)
       :finally (return
                  `(with-prepared-statement (,sql ,connection ,statement)
                     (let ,out-vars 
                       (flet ((,name ,lambda-list
                                (let () ;;,foreign-strings
                                  (unwind-protect
                                       (with-foreign-objects ,foreign-objects
                                         ,@binding
                                         (execute-statement nil ,statement)
                                         ,@get-out-values
                                         (values))
                                    ;; ,@string-cleanup
                                    ))))
                         ,@body)))))))

(defmacro with-stored-proc ((name ora-name) (&rest args) 
                            (&optional (connection '*connection*))
                            &body body)
  `(with-bound-variables (,name ,args)
       (,(format nil "begin ~a(~{:~a~^, ~}); end;" ora-name (mapcar #'first args))
         ,connection)
     ,@body))

(defun execute-statement (&optional sql (statement *statement*))
  (check-errors
   (if sql
       (oci-execute-stmt statement sql)
       (oci-execute statement))))
