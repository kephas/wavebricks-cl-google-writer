(defpackage :wavebricks-cl-google-writer/web
  (:use :cl :alexandria :metabang-bind :cl-json :cl-who :clack :ningle
		:wavebricks-cl-google-writer/google
		:web-object-capabilities/ningle-mongo)
  (:import-from :uiop #:getenv)
  (:import-from :cl-mongo #:parse-mongo-uri #:db.use)
  (:import-from :flexi-streams #:octets-to-string)
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


(defun make-redirect-uri (request key)
  (bind (((&key url-scheme server-name server-port &allow-other-keys) (lack.request:request-env request)))
	(render-uri (make-uri :scheme url-scheme :host server-name :port server-port
						  :path (format nil "/key/~a/oauth" key)))))

(setf (route *app* "/")
	  (lambda (params)
		(declare (ignore params))
		(if-let (key (create-first-time-key!))
		  (let ((url (format nil "/key/~a" key)))
			(page "Wavebricks Google Writer"
			  (:a :href url "Secret admin page")))
		  (page "Wavebricks Google Writer"
			(:p "You need to go to the secret admin page.")))))

(setf (route *app* "/key/:key")
	  (make-key-handler :key ()
		(page "Wavebricks Google Writer — Error" (:p "Key not found."))))

(defmethod handle-document ((type (eql :admin)) document &key &allow-other-keys)
  (let* ((key (get-element "key" document))
		 (redirect-uri (make-redirect-uri *request* key))
		 (action (format nil "/key/~a/setup" key)))
	(set-document-value! document "redirect-uri" redirect-uri)
	(page "Wavebricks Google Writer"
	  (:h1 "Setup")
	  (:p "Redirect URI: " (:code (str redirect-uri)))
	  ((:form :action action :method "POST")
	   (:p "ID: " (:input :type "text" :name "id"))
	   (:p "Secret: " (:input :type "text" :name "secret"))
	   (:p "Spreadsheet: " (:input :type "text" :name "spreadsheet"))
	   (:input :type "submit" :value "Save")))))


(defun key-google-client (key)
  (remake-object 'google-client key '("client")))

(setf (route *app* "/key/:key/setup" :method :post)
	  (lambda (params)
		(let ((id (ningle-param params "id"))
			  (secret (ningle-param params "secret"))
			  (spreadsheet (ningle-param params "spreadsheet"))
			  (key (ningle-param params :key)))
		  (set-document-value! key "spreadsheet" spreadsheet)
		  (let ((client (make-google-client id secret (make-redirect-uri *request* key))))
			(write-object client key '("client"))
			(setf (lack.response:response-status *response*) 302)
			(setf (lack.response:response-headers *response*)
				  (list :location (google-auth-url client)))
			""))))

(setf (route *app* "/key/:key/oauth")
	  (lambda (params)
		(let* ((code (ningle-param params "code"))
			   (key (ningle-param params :key))
			   (client (key-google-client key)))
		  (google-token-request! client code)
		  (write-object client key '("client")))
		(page "Done" (:p "Tokens retrieved."))))


(defun json-request (req)
  (decode-json-from-string (octets-to-string (lack.request:request-content req))))

(defparameter *range* "A1") ; Google Sheets seems to ignore it

(setf (route *app* "/key/:key/callback" :method :post)
	  (lambda-with-key (key :key document)
		  "no such key"
		(bind (((:alist (id :id) (version :version) (type :type) (gw :gateway--id) (base64 :payload--base64)
						(sf :spreading--factor) (freq :frequency) (cr :coding--rate) (rssi :rssi) (snr :snr)
						(bw :bandwidth) (date :received--at))
				(json-request *request*)))
		  (spreadsheet-append! (key-google-client key) (get-element "spreadsheet" document) *range* (list id version type gw base64 sf freq cr rssi snr bw date)))
		""))


(defparameter *default-port* 5555)

(defun server-port ()
  (if-let (port (getenv "PORT")) (parse-integer port) *default-port*))

(defun find-mongo ()
  (if-let (mongo-uri (getenv "MONGODB_URI"))
	(parse-mongo-uri mongo-uri)
	(db.use "wavebricks-cl-google-writer" )))

(defun start-server ()
  (find-mongo)
  (clackup *app* :port (server-port)))
