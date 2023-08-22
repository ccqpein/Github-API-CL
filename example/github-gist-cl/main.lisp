(ql:quickload "github-api-cl")

(defparameter *client* (make-instance 'github-client:api-client :token "token here"))

;;; list all gist of the token's owner
(defun list-gist (client)
  (let ((gist-api-doc (make-instance 'github-api-doc:api-doc
									 :api "GET /gists"
									 :parameters '(("per_page" "integer")
												   ("page" "integer")))))
	
    ;; below api-call will ask :per_page and :page
	;;(github-client:github-api-call client gist-api-doc)

	;; call directly through `github-client:github-api-call`
	(github-client:github-api-call client gist-api-doc :per_page 1 :page 1)
	))


;;; create the gist
(defun create-gist (client)
  (let ((gist-api-doc (make-instance
					   'github-api-doc:api-doc
					   :api "POST /gists"
					   :parameters '())))
	
	(github-client:github-api-call
	 client
	 gist-api-doc
	 :headers '(("Accept" . "application/vnd.github.VERSION.raw")
				("Accept" . "application/vnd.github.VERSION.base64"))
	 :content "{\"description\":\"Example of a gist\",\"public\":false,\"files\":{\"test.md\":{\"content\":\"this is the test of creating gist\"}}}")
	))

;; call like code below
;;(list-gist *client*)
;;(create-gist *client*)
