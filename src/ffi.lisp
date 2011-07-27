(in-package :ocilib)

(defctype bool :int)

(defcenum (env :uint)
  (:oci-env-default  0)
  (:oci-env-threaded 1)
  (:oci-env-context  2)
  (:oci-env-events   4))

(defcfun ("_OCI_Initialize@12" oci-initialize)
    bool
  (error-handler :pointer)
  (lib-path win32:wide-string)
  (mode env))

(defcfun ("_OCI_Cleanup@0" oci-cleanup)
    bool)

(defcfun ("_OCI_EnableWarnings@4" oci-enable-warnings)
    :void
  (enable bool))

(defctype connection :pointer)

(defcenum (mode :uint)
  (:oci-session-default     0)
  (:oci-session-xa          1)
  (:oci-session-sysdba      2)
  (:oci-session-sysoper     4)
  (:oci-session-prelim-auth 8))

(defcfun ("_OCI_ConnectionCreate@16" oci-connection-create)
    connection
  (db win32:wide-string)
  (user win32:wide-string)
  (pwd win32:wide-string)
  (mode mode))

(defcfun ("_OCI_ConnectionFree@4" oci-connection-free)
    bool
  (connection connection))

(defctype statement :pointer)

(defcfun ("_OCI_StatementCreate@4" oci-statement-create)
    statement
  (connection connection))

(defcfun ("_OCI_StatementFree@4" oci-statement-free)
    bool
  (statement statement))

(defcfun ("_OCI_Execute@4" oci-execute)
    bool
  (statement statement))

(defcfun ("_OCI_ExecuteStmt@8" oci-execute-stmt)
    bool
  (statement statement)
  (sql win32:wide-string))

(defcfun ("_OCI_Parse@8" oci-parse)
    bool
  (statement statement)
  (sql win32:wide-string))

(defcfun ("_OCI_GetAffectedRows@4" oci-get-affected-rows)
    bool
  (statement statement))

(defctype result-set :pointer)

(defcfun ("_OCI_GetResultset@4" oci-get-resultset)
    result-set
  (statement statement))

(defcfun ("_OCI_GetRowCount@4" oci-get-row-count)
    :uint
  (result-set result-set))

(defcfun ("_OCI_FetchFirst@4" oci-fetch-first)
    bool
  (result-set result-set))

(defcfun ("_OCI_FetchLast@4" oci-fetch-last)
    bool
  (result-set result-set))

(defcfun ("_OCI_FetchNext@4" oci-fetch-next)
    bool
  (result-set result-set))

(defcfun ("_OCI_FetchPrev@4" oci-fetch-prev)
    bool
  (result-set result-set))

(defcenum (seek-direction :uint)
  (:absolute #x20 )
  (:relative #x40))

(defcfun ("_OCI_FetchSeek@12" oci-fetch-seek)
    bool
  (statement statement)
  (mode seek-direction)
  (offset :int))

(defcfun ("_OCI_GetColumnIndex@8" oci-get-column-index)
    :uint
  (result-set result-set)
  (name win32:wide-string))

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

(defcfun ("_OCI_GetColumn@8" oci-get-column-by-index)
    column
  (result-set result-set)
  (index :uint))

(defcfun ("_OCI_GetColumn2@8" oci-get-column-by-name)
    column
  (result-set result-set)
  (name win32:wide-string))

(defcfun ("_OCI_ColumnGetType@4" oci-get-column-type)
    ora-type
  (column column))

(defcfun ("_OCI_GetInt@8" oci-get-int)
    :int
  (result-set result-set)
  (index :uint))

(defcfun ("_OCI_GetString@8" oci-get-string)
    win32:wide-string
  (result-set result-set)
  (index :uint))

(defcfun ("_OCI_Commit@4" oci-commit)
    bool
  (connection connection))

(defcfun ("_OCI_Rollback@4" oci-rollback)
    bool
  (connection connection))

(defctype oci-error :pointer)

(defcfun ("_OCI_GetLastError@0" oci-get-last-error)
    oci-error)

(defcfun ("_OCI_ErrorGetString@4" oci-error-get-string)
    win32:wide-string
  (error oci-error))
