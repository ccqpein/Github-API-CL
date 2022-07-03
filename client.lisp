;;; client of github api call
(defpackage #:github-client
  (:use #:CL #:github-api-doc)
  (:shadow #:get) ;; shadow get from CL
  (:export #:token-p
           #:token
           #:token-p-or-input
           #:api-client
           #:http-call
           #:github-api-call))

(in-package #:github-client)

(defclass api-client ()
  ((token
    :initarg :token
    :type string
    :initform ""
    :accessor token)))

(defmethod token-p ((clt api-client))
  "check if client has token"
  (declare (api-client clt))
  (not (string= "" (token clt))))

(defmethod token-p-or-input ((clt api-client))
  "check if client has token, if not, ask to input"
  (if (not (token-p clt))
      (progn
        (format t "Please input your token~%")
        (setf (token clt) (read-line))
        )))

(defgeneric http-call (client url &rest args &key method &allow-other-keys))

(defmethod http-call ((clt api-client) url &rest args &key (method "get") content &allow-other-keys)
  (let* ((lambda-list '())
         (call-func (cond
                      ((string= (string-downcase method) "get") #'dex:get)
                      ((string= (string-downcase method) "post")
                       (progn (setf lambda-list
                                    (append lambda-list (list :content content)))
                              #'dex:post))
                      ((string= (string-downcase method) "delete") #'dex:delete)
                      ((string= (string-downcase method) "head") #'dex:head)
                      ((string= (string-downcase method) "put") #'dex:put)
                      ((string= (string-downcase method) "patch") #'dex:patch)
                      ((string= (string-downcase method) "fetch") #'dex:fetch)))
         )
    (destructuring-bind
        (&key
           (token (token clt) token-p)
           (user-name "")
           (passd "" passd-p)
           (proxy "" proxy-p)
         &allow-other-keys)
        args
      (cond
        ;; If has token, use token first
        ;; If has token input, use input token, or use client token
        ((or token-p (token-p clt))
         (setf lambda-list
               (append lambda-list
                       (list :headers (list (cons "Authorization"
                                                  (format nil "token ~a" token)))))))
        
        ;; If neither client's token or keyword token is given
        ;; try use user-name and password
        (passd-p
         (setf lambda-list
               (append lambda-list
                       (list :basic-auth (cons user-name passd)))))

        ;; give proxy
        (proxy-p
         (setf lambda-list
               (append lambda-list
                       (list :proxy proxy :insecure t))))
        )

      (apply call-func url lambda-list)
      )))

(defgeneric github-api-call (client api &rest args &key &allow-other-keys))

;; Except token, user-name, and passd, all other keywords are parameters for this api
(defmethod github-api-call ((clt api-client) (api api-doc)
                            &rest args)
  (let* ((url (apply #'make-call-url api args))
         (parameters (apply #'make-call-parameters api args))
         (whole-url (concatenate 'string url parameters)))
    (apply #'http-call clt whole-url
           :method (http-method api)
           args)
    ))
