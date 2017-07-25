(defpackage :wavebricks-cl-google-writer/google
  (:use :cl :alexandria :cl-json :metabang-bind)
  (:import-from :drakma #:http-request)
  (:import-from :flexi-streams #:octets-to-string)
  (:import-from :quri #:make-uri #:render-uri)
  (:import-from :date-time #:now #:second+ #:date-time<)
  (:export #:make-google-client #:google-auth-url #:google-token-request! #:spreadsheet-append!))

(in-package :wavebricks-cl-google-writer/google)


(defparameter *scope* "https://www.googleapis.com/auth/spreadsheets")


(defclass google-client ()
  ((client-id :initarg :id :reader google-client-id)
   (client-secret :initarg :secret :reader google-client-secret)
   (scopes :initarg :scopes :reader google-scopes)
   (redirect-uri :initarg :uri :reader google-redirect-uri)
   (refresh-token :accessor google-refresh-token)
   (access-token :accessor google-access-token)
   (expiration-time :accessor google-expiration-time)))

(defun make-google-client (id secret uri)
  (make-instance 'google-client :id id :secret secret :scopes *scope* :uri uri))


(defun google-auth-url (client)
  (render-uri
   (make-uri :defaults " https://accounts.google.com/o/oauth2/v2/auth"
			 :query `(("client_id" . ,(google-client-id client))
					  ("redirect_uri" . ,(google-redirect-uri client))
					  ("response_type" . "code")
					  ("scope" . ,*scope*)
					  ("access_type" . "offline")))))

(defun google-update-client! (client response)
  (bind (((:alist (access-token :access--token)
				  (refresh-token :refresh--token)
				  (expiration-delay :expires--in))
		  (decode-json-from-string (octets-to-string response))))
	(when refresh-token
	  (setf (google-refresh-token client) refresh-token))
	(when access-token
	  (setf (google-access-token client) access-token))
	(when expiration-delay
	  (setf (google-expiration-time client) (second+ (now) expiration-delay)))))

(defun google-token-request! (client code)
  (google-update-client!
   client
   (http-request "https://www.googleapis.com/oauth2/v4/token"
				 :method :post
				 :parameters `(("code" . ,code)
							   ("client_id" . ,(google-client-id client))
							   ("client_secret" . ,(google-client-secret client))
							   ("grant_type" . "authorization_code")
							   ("redirect_uri" . ,(google-redirect-uri client))))))

(defun google-refresh-token! (client)
  (google-update-client!
   client
   (http-request "https://www.googleapis.com/oauth2/v4/token"
				 :method :post
				 :parameters `(("refresh_token" . ,(google-refresh-token client))
							   ("client_id" . ,(google-client-id client))
							   ("client_secret" . ,(google-client-secret client))
							   ("grant_type" . "refresh_token")))))

(defun google-updated-token (client)
  (unless (date-time< (now) (google-expiration-time client))
	(google-refresh-token! client))
  (google-access-token client))

					 
(defun spreadsheet-append! (client spreadsheet range values)
  (drakma:http-request
   (render-uri (make-uri :defaults (format nil "https://sheets.googleapis.com/v4/spreadsheets/~a/values/~a:append" spreadsheet range)
						 :query `(("valueInputOption" . "RAW")
								  ("insertDataOption" . "INSERT_ROWS")
								  ("includeValuesInResponse" . "TRUE"))))
   :method :POST
   :content-type "application/json"
   :content (encode-json-plist-to-string (list :range range :major-dimension "ROWS"
											   :values (list values)))
   :additional-headers `(("Authorization" . ,(format nil "Bearer ~a" (google-updated-token client))))))
