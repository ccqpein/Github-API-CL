;;; github api documents
(defpackage #:github-api-doc
  (:use #:CL)
  (:export #:api-doc
           #:http-method
           #:read-api-json
           #:make-api-doc-from-json
           #:make-call-parameters
           #:make-call-url)
  )

(in-package #:github-api-doc)

(defparameter *api-root-url* "https://api.github.com")
(defparameter *api-json-file-path*
  (merge-pathnames (asdf:component-pathname
                    (asdf:find-system :github-api-cl))
                   #P"/api.json")
  "api json file path")

(defun read-api-json (&optional (file-path *api-json-file-path*))
  "read json api file from *api-json-file-path*, return yason json
object"
  (declare (pathname file-path))
  (with-open-file (s file-path)
    (let ((data (make-string (file-length s))))
      (read-sequence data s)
      (yason:parse data))))

(defun make-api-doc-from-json (json-obj &rest paths)
  "call with read-api-json usually"
  (let ((api-detail (dolist (p paths json-obj)
                      (setf json-obj (gethash p json-obj)))))
    (make-instance 'api-doc
                   :api (gethash "api" api-detail)
                   :parameters (gethash "parameters" api-detail))))

(defclass api-doc ()
  ((api
    :initarg :api
    :type string
    :accessor api)

   (http-method
    :type string
    :accessor http-method)

   ;; (:slot-name...)
   (slots
    :type cons
    :accessor slots)

   (fmt-control
    :type string
    :accessor control-str)

   ;; ((para, type)...)
   (parameters
    :initarg :parameters
    :initform '()
    :type cons
    :accessor parameters)))

(defmethod initialize-instance :after ((api api-doc) &key)
  (let ((api-detail (parse-api (api api))))
    ;; update method
    (setf (http-method api) (car api-detail)
          (control-str api) (cadr api-detail))

    (setf (slots api)
          (cddr api-detail))
    ))

(defun parse-api (str)
  "give an api entry, return (method format-control slots)"
  (declare (string str))
  (let* ((a (str:split " " str))
         (http-method (car a))
         ;; split api url
         (b (str:split "/" (cadr a)))
         (fmt-control '())
         (cache '())
         (slots '()))
    
    (dolist (x b)
      (unless (string= "" x)
        (push "/" cache)
        (if (str:starts-with? ":" x)
            (progn (push "~a" cache)
                   (push x slots)
                   (push (str:join "" (reverse cache)) fmt-control)
                   (setf cache '())
                   )
            (push x cache))))

    (push (str:join "" (reverse cache)) fmt-control)
    
    (the (cons string *)
         (append
          (list http-method
                (reverse fmt-control))
          (reverse slots)))
    ))

(defmethod print-object ((api api-doc) stream)
  (format stream
          "api-doc object:
  api: ~a,
  http method: ~a,
  slots: ~a,
  fmt-control: ~a
  parameters: ~a"
          (api api) (http-method api) (slots api) (control-str api) (parameters api)
          ))

(defgeneric make-call-url (api &rest args &key &allow-other-keys)
  (:documentation "Return the url of this api http call"))

(defmethod make-call-url ((api api-doc) &rest args &key (root *api-root-url*) &allow-other-keys)
  "make the url of this api ask user to input data and return call
url"
  (let ((result (make-string-output-stream))
        (slot-finder (parse-keys args))) ;; make slot finder funtion
    (format result root)
    (loop
      for k in (slots api)
      for ss in (control-str api)
      for v = (funcall slot-finder k)
      do (format result ss v) ;; push url to result one by one
      finally (format result
                      (car (last (control-str api)))))

    (the string (get-output-stream-string result))
    ))

(declaim (inline parse-keys))
(defun parse-keys (args)
  "for make parse keywords function, make flexible of keywords input"
  (if (not (evenp (length args)))
      (return-from
       parse-keys (error "args length should be even")))
  
  (let ((keywords-pairs (loop
                          for i from 2 to (length args) by 2
                          collect (subseq args (- i 2) i))))
    (lambda (slot)
      (declare (string slot))
      (let* ((k-slot (read-from-string slot)) ;; make string to keyword
             (pair (find-if #'(lambda (pair) (eql (car pair) k-slot))
                            keywords-pairs)))
        (if pair
            (cadr pair)
            (progn (format t "What's ~a: " slot)
                   (string-downcase (read-line))))))))

;; ss should be integar or string
(declaim (inline coerce-parameter-type))
(defun coerce-parameter-type (ss type-ss)
  (declare (string type-ss))
  (cond
    ((not ss) nil)
    
    ((string= type-ss "string")
     ss)
    
    ((string= type-ss "boolean") ;; it might be "true", "false", or ""
     (cond
       ((string= ss "true") ss)
       ((string= ss "false") ss)
       (t ""))) 

    ;; integer can be integer from keyword, or string from (read-line)
    ;; return "" instead of 0 is fine. It will be cleaned anyway.
    ((string= type-ss "integer")
     (if (integerp ss) ;; if from keyword ss is int
         ss
         (if (string/= ss "")
             (parse-integer ss)
             ss)))
    
    (t ss)))

(defmethod make-call-parameters ((api api-doc) &rest args &key &allow-other-keys)
  (let (values-list
        (parameters-str (make-string-output-stream)))
    (setf values-list
          (if (zerop (length args))
              ;; if input nothing, ask one by one
              ;; all values input are string
              (loop 
                for (pa type) in (parameters api)
                collect (progn (format t "What's ~a (type is ~a)?: " pa type)
                               (list pa (coerce-parameter-type
                                         (string-downcase (read-line))
                                         type))))

              ;; else, parse keyword
              (let ((keywords-pairs (loop
                                      for i from 2 to (length args) by 2
                                      collect (subseq args (- i 2) i))))
                ;; keywords-pairs = ((keyword val)...)
                (loop
                  for (pa type) in (parameters api)
                  collect (list pa (coerce-parameter-type
                                    (cadr
                                     (find-if #'(lambda (x)
                                                  (equal (string-downcase
                                                          (string
                                                           (car x)))
                                                         pa))
                                              keywords-pairs))
                                    type))))
              ))
    ;; make parameters
    (format parameters-str
            "?~{~#[~:;~{~a=~s~}~#[~:;&~]~]~}"
            ;; clean all list if value is empty or nil
            (loop
              ;; check the value legal or not. Clean nil and ""
              with check-val = (lambda (x)
                                 (if (stringp x)
                                     (string/= x "")
                                     (if x x)))
              for (p v) in values-list
              when (funcall check-val v)
                collect (list p v)))    
    (the string (get-output-stream-string parameters-str))))
