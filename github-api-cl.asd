;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:github-api-cl-sys
  (:use #:CL #:asdf))

(in-package #:github-api-cl-sys)

(defsystem github-api-cl
  :name "github-api-cl"
  :version (:read-file-form "version")
  :author "ccQpein"
  :maintainer "ccQpein"

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
  )

(defmethod perform ((o test-op) (c (eql (find-system :github-api-cl))))
  (load-system :github-api-cl/tests)
  ;;(oos 'test-op :github-api-cl/tests)
  )

(defsystem github-api-cl/tests
  :defsystem-depends-on ("lisp-unit")
  :components ((:file "api-doc-test")
               (:file "client-test")))
