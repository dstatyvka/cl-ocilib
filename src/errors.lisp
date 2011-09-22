(in-package :ocilib)

(defun describe-last-error ()
  (let ((oci-error (oci-get-last-error)))
    (if (null-pointer-p oci-error)
        (error "Unknown OCI error")
        (error "OCI error: ~a"
               (oci-error-get-string oci-error)))))

(defun check-errors (last-result)
  (cond
    ((numberp last-result)
     (or (not (zerop last-result))
         (describe-last-error)))

    ((null-pointer-p last-result)
     (describe-last-error))

    (t last-result)))
