(defpackage #:api-doc-test
  (:use #:CL #:lisp-unit)
  (:import-from #:github-api-doc
                #:api-doc
                #:make-call-parameters
                #:make-call-url))

(in-package #:api-doc-test)

(define-test coerce-parameter-type-test
  (assert-equal "" (github-api-doc::coerce-parameter-type "" "string"))
  (assert-equal "aa" (github-api-doc::coerce-parameter-type "aa" "string"))

  ;; from (read-line) or keywords
  (assert-equal "" (github-api-doc::coerce-parameter-type "" "boolean"))
  (assert-equal "false" (github-api-doc::coerce-parameter-type "false" "boolean"))
  (assert-equal "true" (github-api-doc::coerce-parameter-type "true" "boolean"))

  ;; from keyword
  (assert-equal 1 (github-api-doc::coerce-parameter-type 1 "integer"))
  ;; from (read-line)
  (assert-equal 12 (github-api-doc::coerce-parameter-type "12" "integer"))
  (assert-equal "" (github-api-doc::coerce-parameter-type "" "integer")) ;; empty (read-line)
  )

(define-test make-call-parameters-test
  (let ((api-doc (make-instance 'api-doc
                                :api "POST /user/repos"
                                :parameters '(("name" "string")
                                              ("private" "boolean")
                                              ("team_id" "integer")))))
    ;; fake input from repl
    (assert-equal "?name=aa&private=true&team_id=1"
                  (with-input-from-string (*standard-input* "aa
true
1")
                    (make-call-parameters api-doc)))

    ;; input by keywords, ignore wrong parameters
    (assert-equal "?private=true"
                  (make-call-parameters api-doc :username "aa" :private "true" :integer 1))

    ;; input by keywords
    (assert-equal "?name=aa&private=true&team_id=1"
                  (make-call-parameters api-doc :name "aa" :private "true" :team_id 1))

	;; empty parameters
	(progn (setf api-doc (make-instance 'api-doc
                                :api "POST /user/repos"
								:parameters '()))
		   (assert-equal ""
                  (make-call-parameters api-doc :some "a")))
    ))

(define-test make-call-url-test
  (let ((api-doc (make-instance 'api-doc
                                :api "GET /user/:repo/:aaa/:id")))

    (assert-equal "https://api.github.com/user/aa/bb/3"
                  (with-input-from-string (*standard-input* "aa
bb
3")
                    (make-call-url api-doc)))

    (assert-equal "https://api.github.com/user/aa/bb/3"
                  (make-call-url api-doc :repo "aa" :aaa "bb" :id 3))))

(let ((*print-errors* t)
      (*print-failures* t))
  (run-tests :all :api-doc-test))
