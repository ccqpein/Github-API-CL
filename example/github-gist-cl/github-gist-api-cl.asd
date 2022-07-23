(defpackage #:github-gist-api-cl-sys
  (:use #:CL #:asdf))

(in-package #:github-gist-api-cl-sys)

(defsystem github-gist-api-cl
  :name "github-gist-api-cl"
  ;;:version (:read-file-form "version")
  :defsystem-depends-on ("github-api-cl")
  )
