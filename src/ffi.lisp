(in-package :ocilib)

(defctype bool :int)

(defcenum (env :uint)
  (:oci-env-default  0)
  (:oci-env-threaded 1)
  (:oci-env-context  2)
  (:oci-env-events   4))

(defcfun (#+ocilib32 "_OCI_Initialize@12"
                     #-ocilib32 "OCI_Initialize" oci-initialize)
    bool
  (error-handler :pointer)
  (lib-path win32:wide-string)
  (mode env))

(defcfun (#+ocilib32 "_OCI_Cleanup@0"
                     #-ocilib32 "OCI_Cleanup" oci-cleanup)
    bool)

(defcfun (#+ocilib32 "_OCI_EnableWarnings@4"
                     #-ocilib32 "OCI_EnableWarnings" oci-enable-warnings)
    :void
  (enable bool))

(defctype connection :pointer)

(defcenum (mode :uint)
  (:oci-session-default     0)
  (:oci-session-xa          1)
  (:oci-session-sysdba      2)
  (:oci-session-sysoper     4)
  (:oci-session-prelim-auth 8))

(defcfun (#+ocilib32 "_OCI_ConnectionCreate@16"
                     #-ocilib32 "OCI_ConnectionCreate" oci-connection-create)
    connection
  (db win32:wide-string)
  (user win32:wide-string)
  (pwd win32:wide-string)
  (mode mode))

(defcfun (#+ocilib32 "_OCI_ConnectionFree@4"
                     #-ocilib32 "OCI_ConnectionFree" oci-connection-free)
    bool
  (connection connection))

(defctype statement :pointer)

(defcfun (#+ocilib32 "_OCI_StatementCreate@4"
                     #-ocilib32 "OCI_StatementCreate" oci-statement-create)
    statement
  (connection connection))

(defcfun (#+ocilib32 "_OCI_StatementFree@4"
                     #-ocilib32 "OCI_StatementFree" oci-statement-free)
    bool
  (statement statement))

(defcfun (#+ocilib32 "_OCI_Execute@4"
                     #-ocilib32 "OCI_Execute" oci-execute)
    bool
  (statement statement))

(defcfun (#+ocilib32 "_OCI_ExecuteStmt@8"
                     #-ocilib32 "OCI_ExecuteStmt" oci-execute-stmt)
    bool
  (statement statement)
  (sql win32:wide-string))

(defcfun (#+ocilib32 "_OCI_Parse@8"
                     #-ocilib32 "OCI_Parse" oci-parse)
    bool
  (statement statement)
  (sql win32:wide-string))

(defcfun (#+ocilib32 "_OCI_GetAffectedRows@4"
                     #-ocilib32 "OCI_GetAffectedRows" oci-get-affected-rows)
    bool
  (statement statement))

(defctype result-set :pointer)

(defcfun (#+ocilib32 "_OCI_GetResultset@4"
                     #-ocilib32 "OCI_GetResultset" oci-get-resultset)
    result-set
  (statement statement))

(defcfun (#+ocilib32 "_OCI_GetRowCount@4"
                     #-ocilib32 "OCI_GetRowCount" oci-get-row-count)
    :uint
  (result-set result-set))

(defcfun (#+ocilib32 "_OCI_FetchFirst@4"
                     #-ocilib32 "OCI_FetchFirst" oci-fetch-first)
    bool
  (result-set result-set))

(defcfun (#+ocilib32 "_OCI_FetchLast@4"
                     #-ocilib32 "OCI_FetchLast" oci-fetch-last)
    bool
  (result-set result-set))

(defcfun (#+ocilib32 "_OCI_FetchNext@4"
                     #-ocilib32 "OCI_FetchNext" oci-fetch-next)
    bool
  (result-set result-set))

(defcfun (#+ocilib32 "_OCI_FetchPrev@4"
                     #-ocilib32 "OCI_FetchPrev" oci-fetch-prev)
    bool
  (result-set result-set))

(defcenum (seek-direction :uint)
  (:absolute #x20 )
  (:relative #x40))

(defcfun (#+ocilib32 "_OCI_FetchSeek@12"
                     #-ocilib32 "OCI_FetchSeek" oci-fetch-seek)
    bool
  (statement statement)
  (mode seek-direction)
  (offset :int))

(defcfun (#+ocilib32 "_OCI_GetColumnIndex@8"
                     #-ocilib32 "OCI_GetColumnIndex" oci-get-column-index)
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

(defcfun (#+ocilib32 "_OCI_GetColumn@8"
                     #-ocilib32 "OCI_GetColumn" oci-get-column-by-index)
    column
  (result-set result-set)
  (index :uint))

(defcfun (#+ocilib32 "_OCI_GetColumn2@8"
                     #-ocilib32 "OCI_GetColumn2" oci-get-column-by-name)
    column
  (result-set result-set)
  (name win32:wide-string))

(defcfun (#+ocilib32 "_OCI_ColumnGetType@4"
                     #-ocilib32 "OCI_ColumnGetType" oci-get-column-type)
    ora-type
  (column column))

(defcfun (#+ocilib32 "_OCI_GetInt@8"
                     #-ocilib32 "OCI_GetInt" oci-get-int)
    :int
  (result-set result-set)
  (index :uint))

(defcfun (#+ocilib32 "_OCI_GetString@8"
                     #-ocilib32 "OCI_GetString" oci-get-string)
    win32:wide-string
  (result-set result-set)
  (index :uint))

(defcfun (#+ocilib32 "_OCI_Commit@4"
                     #-ocilib32 "OCI_Commit" oci-commit)
    bool
  (connection connection))

(defcfun (#+ocilib32 "_OCI_Rollback@4"
                     #-ocilib32 "OCI_Rollback" oci-rollback)
    bool
  (connection connection))

(defctype oci-error :pointer)

(defcfun (#+ocilib32 "_OCI_GetLastError@0"
                     #-ocilib32 "OCI_GetLastError" oci-get-last-error)
    oci-error)

(defcfun (#+ocilib32 "_OCI_ErrorGetString@4"
                     #-ocilib32 "OCI_ErrorGetString" oci-error-get-string)
    win32:wide-string
  (error oci-error))
