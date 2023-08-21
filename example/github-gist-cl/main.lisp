(ql:quickload "github-api-cl")

;;; make a new class for gist api
;;; you dont have to do this if you just want to get instead of post 
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

;;; list all gist of the token's owner
(defun list-gist (client)
  (let ((gist-api-doc (make-instance 'gist-api-doc :api "GET /gists"
												   :parameters '(("per_page" "integer")
																 ("page" "integer")))))
	
    ;; below api-call will ask :per_page and :page because in defmethod, we don't give arguments
	;;(api-call client gist-api-doc :per_page 1 :page 1)

	;; call directly through `github-client:github-api-call`
	(github-client:github-api-call client gist-api-doc :per_page 1 :page 1)
	))


;;; create the gist
(defun create-gist (client)
  (let ((gist-api-doc (make-instance 'gist-api-doc :api "POST /gists"
												   :parameters '()
												   :content "{\"description\":\"Example of a gist\",\"public\":false,\"files\":{\"test.md\":{\"content\":\"this is the test of creating gist\"}}}")))
	
	(api-call client gist-api-doc)
	))
