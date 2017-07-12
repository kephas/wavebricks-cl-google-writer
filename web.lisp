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

(defun ningle-param (params name) (cdr (assoc name params)))


(defparameter *redirect-path* "/oauth")

(defun local-redirect-uri (request)
  (bind (((&key url-scheme server-name server-port &allow-other-keys) (lack.request:request-env request)))
	(render-uri (make-uri :scheme url-scheme :host server-name :port server-port
						  :path *redirect-path*))))

(setf (route *app* "/")
	  (lambda (params)
		(declare (ignore params))
		(page "Wavebricks Google Writer"
		  (:h1 "Setup")
		  (:p "Redirect URI: " (:code (str (local-redirect-uri *request*))))
		  ((:form :action "/setup" :method "POST")
		   (:p "ID: " (:input :type "text" :name "id"))
		   (:p "Secret: " (:input :type "text" :name "secret"))
		   (:input :type "button" :value "Save")))))


(defparameter *default-port* 5555)

(defun server-port ()
  (if-let (port (getenv "PORT")) (parse-integer port) *default-port*))

(defun start-server ()
  (clackup *app* :port (server-port)))
