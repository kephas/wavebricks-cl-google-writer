(defpackage :wavebricks-cl-google-writer/proto
  (:use :cl :alexandria :cl-json)
  (:import-from :drakma #:http-request)
  (:import-from :flexi-streams #:octets-to-string)
  (:import-from :quri #:make-uri #:render-uri))

(in-package :wavebricks-cl-google-writer/proto)


(defparameter *scope* "https://www.googleapis.com/auth/spreadsheets")


(defclass google-client ()
  ((client-id :initarg :id :reader google-client-id)
   (client-secret :initarg :secret :reader google-client-secret)
   (scopes :initarg :scopes :reader google-scopes)
   (redirect-uri :initarg :uri :reader google-redirect-uri)))


(defun google-auth-url (client)
  (render-uri
   (make-uri :defaults " https://accounts.google.com/o/oauth2/v2/auth"
			 :query `(("client_id" . ,(google-client-id client))
					  ("redirect_uri" . ,(google-redirect-uri client))
					  ("response_type" . "code")
					  ("scope" . ,*scope*)
					  ("access_type" . "offline")))))

(defun google-token-request (client code)
  (decode-json-from-string
   (octets-to-string
	(http-request "https://www.googleapis.com/oauth2/v4/token"
				  :method :post
				  :parameters `(("code" . ,code)
								("client_id" . ,(google-client-id client))
								("client_secret" . ,(google-client-secret client))
								("grant_type" . "authorization_code")
								("redirect_uri" . ,(google-redirect-uri client)))))))
					 
(defun spreadsheet-append (spreadsheet range values)
  (drakma:http-request
   (render-uri (make-uri :defaults (format nil "https://sheets.googleapis.com/v4/spreadsheets/~a/values/~a:append" spreadsheet range)
						 :query `(("key" . ,*api-key*))))
   :method :POST
   :content-type "application/json"
   :content (encode-json-plist-to-string (list :range range :major-dimension "ROWS"
											   :values (list values)))))
