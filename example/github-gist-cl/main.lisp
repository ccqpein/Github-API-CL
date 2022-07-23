(ql:quickload "github-api-cl")

(defclass gist-api-doc (github-api-doc:api-doc)
  ((content
    :initarg :content
    :type string
    :initform ""
    :accessor content
    :documentation "content send to api")))

(defmethod content-p ((api gist-api-doc))
  (not (string= (content api) "")))

;; make method that input the content
(defmethod input-content ((api gist-api-doc))
  (progn (format t "What is content?~%")
         (string-downcase (read-line)))
  )

;; just make the github-api-cl:api-client
(defun make-api-client (&key token)
  (if token
      (make-instance 'github-client:api-client :token token)
      (make-instance 'github-client:api-client)))

(defmethod api-call ((client github-client:api-client) (api gist-api-doc)
                     &rest args)
  (if (content-p api)
      (github-client:github-api-call client api
                                     ;; add headers of github gist api use
                                     :headers '(("Accept" . "application/vnd.github.VERSION.raw")
                                                ("Accept" . "application/vnd.github.VERSION.base64"))
                                     :content (content api))
      (github-client:github-api-call client api)
      ))
