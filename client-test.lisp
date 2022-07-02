(defpackage #:client-test
  (:use #:CL #:lisp-unit)
  (:import-from #:github-client
                #:token-p
                #:token
                #:token-p-or-input
                #:api-client
                #:http-call
                #:github-api-call))

(in-package #:client-test)

(define-test token-p-test
  (assert-false (token-p
                 (make-instance 'api-client)))
  
  (assert-true (token-p
                (make-instance 'api-client :token "123"))))

;; make fake http server and receive http-call
(define-test http-call-test
  (let (handler
        env
        (clt (make-instance 'api-client
                            :token "123")))
    (unwind-protect
         (progn (setf handler
                      (clack:clackup
                       (lambda (request)
                         (setf env request) ;; update env
                         (list 200
                               '(:content-type "text/plain")
                               (list (gethash "authorization" (car (last env))))))
                       :server :woo))

                ;; default method is :GET
                (assert-equal :GET
                              (progn (http-call clt "http://127.0.0.1:5000")
                                     (alexandria:doplist (k v env)
                                       (if (eq k :REQUEST-METHOD)
                                           (return v)))))

                ;; give some method
                (assert-equal :DELETE
                              (progn (http-call clt
                                                "http://127.0.0.1:5000"
                                                :method "delete")
                                     (alexandria:doplist (k v env)
                                       (if (eq k :REQUEST-METHOD)
                                           (return v)))))

                ;; give token
                (assert-equal "token 456"
                              (car (multiple-value-list
                                    (http-call clt
                                               "http://127.0.0.1:5000"
                                               :token "456"))))

                ;; don't give token, use client token
                (assert-equal "token 123"
                              (car (multiple-value-list
                                    (http-call clt
                                               "http://127.0.0.1:5000"))))
                
                ;; :content keyword will give the body
                ;; (assert-equal ""
                ;;               (progn (http-call clt
                ;;                                 "http://127.0.0.1:5000"
                ;;                                 :method "delete")
                ;;                      (alexandria:doplist (k v env)
                ;;                        (if (eq k :REQUEST-METHOD)
                ;;                            (return v)))))
                
                ;; give username and passd
                (setf (github-client::token clt) "") ;; empty token first
                (assert-equal (format nil "Basic ~a"
                                      (cl-base64:string-to-base64-string "aa:bb"))
                              (car (multiple-value-list
                                    (http-call clt
                                               "http://127.0.0.1:5000"
                                               :user-name "aa"
                                               :passd "bb")))))
      
      (clack:stop handler))))

(let ((*print-errors* t)
      (*print-failures* t))
  (run-tests :all :client-test))
