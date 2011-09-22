(in-package :ocilib)

(defmacro def-oci-fun (name return-type &body args)
  (flet ((split-by-dash (string)
           (loop
              :for start = 0 :then (1+ end)
              :for end = (position #\- string :start start)
              :collect (subseq string start end)
              :while end)))
    (let* ((parts (split-by-dash (symbol-name name)))
           (lisp-name (intern (format nil "OCI-~{~:@(~a~^-~)~}" parts)
                              (symbol-package name)) )
           (c-name #-(and win32 x86)
                   (format nil "OCI_~{~:(~a~)~}" parts)
                   
                   #+(and win32 x86)
                   (format nil "_OCI_~{~:(~a~)~}@~a" parts
                           (loop :for (arg type) :in args
                              :sum (cffi:foreign-type-size type)))))
      `(defcfun (,c-name ,lisp-name) ,return-type
         ,@args))))

(defctype oci-string 
    #-win32 :string
    #+win32(:string :encoding :ucs-2/le))

(defctype bool :int)

(defcenum (env :uint)
  (:oci-env-default  0)
  (:oci-env-threaded 1)
  (:oci-env-context  2)
  (:oci-env-events   4))

(def-oci-fun initialize bool
  (error-handler :pointer)
  (lib-path oci-string)
  (mode env))

(def-oci-fun cleanup bool)

(def-oci-fun enable-warnings :void
  (enable bool))

(defctype connection :pointer)

(defcenum (mode :uint)
  (:oci-session-default     0)
  (:oci-session-xa          1)
  (:oci-session-sysdba      2)
  (:oci-session-sysoper     4)
  (:oci-session-prelim-auth 8))

(def-oci-fun connection-create connection
  (db oci-string)
  (user oci-string)
  (pwd oci-string)
  (mode mode))

(def-oci-fun connection-free bool
  (connection connection))

(defctype statement :pointer)

(def-oci-fun statement-create statement
  (connection connection))

(def-oci-fun statement-free bool
  (statement statement))

(def-oci-fun execute bool
  (statement statement))

(def-oci-fun execute-stmt bool
  (statement statement)
  (sql oci-string))

(def-oci-fun prepare bool
  (statement statement)
  (sql oci-string))

(def-oci-fun parse bool
  (statement statement)
  (sql oci-string))

(def-oci-fun get-affected-rows bool
  (statement statement))

(defctype result-set :pointer)

(def-oci-fun get-resultset result-set
  (statement statement))

(def-oci-fun get-row-count :uint
  (result-set result-set))

(def-oci-fun fetch-first bool
  (result-set result-set))

(def-oci-fun fetch-last bool
  (result-set result-set))

(def-oci-fun fetch-next bool
  (result-set result-set))

(def-oci-fun fetch-prev bool
  (result-set result-set))

(defcenum (seek-direction :uint)
  (:absolute #x20 )
  (:relative #x40))

(def-oci-fun fetch-seek bool
  (statement statement)
  (mode seek-direction)
  (offset :int))

(def-oci-fun get-column-index :uint
  (result-set result-set)
  (name oci-string))

(defcenum (ora-type :uint)
  (:numeric    1)
  (:datetime   3)
  (:text       4)
  (:long       5)
  (:cursor     6)
  (:lob        7)
  (:file       8)
  (:timestamp  9)
  (:interval   10)
  (:raw        11)
  (:object     12)
  (:collection 13)
  (:ref        14))

(defctype column :pointer)

(def-oci-fun get-column column
  (result-set result-set)
  (index :uint))

(def-oci-fun get-column2 column
  (result-set result-set)
  (name oci-string))

(def-oci-fun column-get-type ora-type
  (column column))

(def-oci-fun get-int :int
  (result-set result-set)
  (index :uint))

(def-oci-fun get-string oci-string
  (result-set result-set)
  (index :uint))

(def-oci-fun commit bool
  (connection connection))

(def-oci-fun rollback bool
  (connection connection))

(def-oci-fun bind-int bool
  (statement statement)
  (name oci-string)
  (data :pointer))

(def-oci-fun bind-array-of-ints bool
  (statement statement)
  (name oci-string)
  (data :pointer)
  (count :uint))

(def-oci-fun bind-unsigned-int bool
  (statement statement)
  (name oci-string)
  (data :pointer))

(def-oci-fun bind-array-of-unsigned-ints bool
  (statement statement)
  (name oci-string)
  (data :pointer)
  (count :uint))

(def-oci-fun bind-big-int bool
  (statement statement)
  (name oci-string)
  (data :pointer))

(def-oci-fun bind-array-of-big-ints bool
  (statement statement)
  (name oci-string)
  (data :pointer)
  (count :uint))

(def-oci-fun bind-string bool
  (statement statement)
  (name oci-string)
  (data :pointer)
  (len :uint))

(def-oci-fun bind-array-of-strings bool
  (statement statement)
  (name oci-string)
  (data :pointer)
  (len :uint)
  (count :uint))

(defctype oci-bind :pointer)

(def-oci-fun get-bind oci-bind
  (statement statement)
  (index :uint))

(def-oci-fun get-bind2 oci-bind
  (statement statement)
  (name oci-string))


(defcenum (bind-mode :uint)
  (:oci-bind-by-pos 0)
  (:oci-bind-by-name 1))

(def-oci-fun set-bind-mode oci-bind
  (statement statement)
  (mode bind-mode))


(defcenum (bind-direction :uint)
  (:in  1)
  (:out 2)
  (:in-out 3))

(def-oci-fun bind-get-direction bind-direction
  (binding oci-bind))

(def-oci-fun bind-set-direction bool
  (binding oci-bind)
  (direction bind-direction))

(def-oci-fun bind-set-null bool
  (binding oci-bind))

(def-oci-fun bind-is-null bool
  (binding oci-bind))

(defctype oci-error :pointer)

(def-oci-fun get-last-error oci-error)

(def-oci-fun error-get-string oci-string
  (error oci-error))

(def-oci-fun error-get-statement statement
  (error oci-error))

