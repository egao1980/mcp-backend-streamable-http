;;;; Phase 1: install SUT dependency closure via cl-repository-client.
;;;; No Quicklisp — OCI only (egao1980/cl-systems).

(setf *debugger-hook*
      (lambda (c h)
        (declare (ignore h))
        (format *error-output* "~&UNHANDLED: ~A~%" c)
        (uiop:quit 1)))

(setf asdf:*compile-file-failure-behaviour* :warn)

(defun call-with-ci-muffles (fn)
  #+sbcl
  (handler-bind ((sb-ext:defconstant-uneql #'continue))
    (funcall fn))
  #-sbcl
  (funcall fn))

(call-with-ci-muffles (lambda () (asdf:load-system "cl-repository-client")))

(cl-repo:add-registry "https://ghcr.io" :namespace "egao1980/cl-systems" :priority :prepend)

(call-with-ci-muffles
 (lambda ()
   (cl-repo:ensure-system-dependencies "mcp-backend-streamable-http"
     :also-tests t
     :default-source :oci
     :with '("dissect")
     :sources '(("dissect" :ql)))))

(format t "~&; ci: install phase done~%")
(uiop:quit 0)
