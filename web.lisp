(defpackage :wavebricks-cl-google-writer/web
  (:use :cl :alexandria :metabang-bind :cl-who :clack :ningle :wavebricks-cl-google-writer/google)
  (:import-from :uiop #:getenv)
  (:import-from :quri #:make-uri #:render-uri))

(in-package :wavebricks-cl-google-writer/web)

(defvar *app* (make-instance '<app>))

(defmacro page (title &body content)
  (once-only (title)
	`(with-html-output-to-string (out)
	   (htm
		(:html
		 (:head (:title (str ,title)))
		 (:body
		  ,@content))))))

(defun ningle-param (params name) (cdr (assoc name params :test 'equal)))


(defparameter *redirect-path* "/oauth")
(defvar *redirect-uri*)

(defun local-redirect-uri (request)
  (bind (((&key url-scheme server-name server-port &allow-other-keys) (lack.request:request-env request)))
	(render-uri (make-uri :scheme url-scheme :host server-name :port server-port
						  :path *redirect-path*))))

(setf (route *app* "/")
	  (lambda (params)
		(declare (ignore params))
		(setf *redirect-uri* (local-redirect-uri *request*))
		(page "Wavebricks Google Writer"
		  (:h1 "Setup")
		  (:p "Redirect URI: " (:code (str *redirect-uri*)))
		  ((:form :action "/setup" :method "POST")
		   (:p "ID: " (:input :type "text" :name "id"))
		   (:p "Secret: " (:input :type "text" :name "secret"))
		   (:input :type "submit" :value "Save")))))

(defvar *google-client*)

(setf (route *app* "/setup" :method :post)
	  (lambda (params)
		(let ((id (ningle-param params "id"))
			  (secret (ningle-param params "secret")))
		  (setf *google-client* (make-google-client id secret *redirect-uri*))
		  (setf (lack.response:response-status *response*) 302)
		  (setf (lack.response:response-headers *response*)
				(list :location (google-auth-url *google-client*)))
		  "")))

(setf (route *app* *redirect-path*)
	  (lambda (params)
		(let ((code (ningle-param params "code")))
		  (google-token-request! *google-client* code))
		(page "Done" (:p "Tokens retrieved."))))


(defparameter *default-port* 5555)

(defun server-port ()
  (if-let (port (getenv "PORT")) (parse-integer port) *default-port*))

(defun start-server ()
  (clackup *app* :port (server-port)))
