(defpackage :wavebricks-cl-google-writer-system
  (:use :common-lisp :asdf))

(in-package :wavebricks-cl-google-writer-system)

(defsystem "wavebricks-cl-google-writer"
  :description "LoRaWAN application server that appends packet data to a Google spreadsheet"
  :version "0.1"
  :author "Pierre Thierry <pierre@nothos.net>"
  :class :package-inferred-system
  :depends-on ("wavebricks-cl-google-writer/proto")
  :licence "AGPL")