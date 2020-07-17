(ql:quickload "github-api-cl")

(defparameter *api-docs*
  (github-api-doc:read-api-json #P"./find-repo-releases.json"))

(defparameter *client*
  (make-instance 'github-client:api-client))

(let* ((respone (car (multiple-value-list
                      (github-client:github-api-call
                       *client*
                       (github-api-doc:make-api-doc-from-json *api-docs* "get a release")
                       :owner "rust-analyzer"
                       :repo "rust-analyzer"
                       :release_id "latest"))))
       (table (yason:parse respone)))
  (format t "~a" respone))

