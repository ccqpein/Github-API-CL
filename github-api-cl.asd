;;;; -*- Mode: Lisp -*-

(defpackage #:github-api-cl-sys
  (:use #:CL #:asdf))

(in-package #:github-api-cl-sys)

(defsystem github-api-cl
  :name "github-api-cl"
  :version (:read-file-form "version")
  :author "ccQpein"
  :maintainer "ccQpein"
  :license "Apache"
  :homepage "https://github.com/ccqpein/Github-API-CL"
  :bug-tracker "https://github.com/ccqpein/Github-API-CL/issues"
  :source-control (:git "git@github.com:ccqpein/Github-API-CL.git")
  :description "The lite Github rest v3 api client SDK"
  :defsystem-depends-on ("str"
                         "yason"
                         "dexador"
                         "woo"
                         "clack"
                         "alexandria"
                         "cl-base64")
  :components ((:file "api-doc")
               (:file "client"
                :depends-on ("api-doc")))
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "github-api-cl/tests")))
  )

(defsystem github-api-cl/tests
  :depends-on ("github-api-cl" "lisp-unit")
  :components ((:file "api-doc-test")
               (:file "client-test"))
  )
