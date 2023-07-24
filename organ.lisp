(defpackage :organ
  (:use :cl :cl-ppcre)
  (:import-from :uiop :read-file-string)
  (:shadowing-import-from :sb-gray :fundamental-stream)
  (:export
   :org-heading-regexp
   :org-file-property-regexp
   :org-property-regexp
   :org-element-types
   :org-element-objects
   :org-file
   :read-org-file
   :read-org-slice
   :org-slice
   :org-stream))

(in-package :organ)

(defvar org-headline-regexp (cl-ppcre:parse-string "^([*]+)\\s+(.*)$"))
(defvar org-file-property-regexp (cl-ppcre:parse-string "^[#+](.*)[:]\\s+(.*)$"))
(defvar org-property-regexp (cl-ppcre:parse-string "^[:](.*)[:]\\s+(.*)$"))

(defvar org-element-types
  '(babel-call center-block clock comment comment-block diary-sexp drawer
    dynamic-block example-block export-block fixed-width footnote-definition
    headline horizontal-rule inlinetask item keyword latex-environment
    node-property paragraph plain-list planning property-drawer quote-block
    section special-block src-block table table-row verse-block)
  "List of all org-element types provided by org-element.el in 'org-element-all-elements'")

(defvar org-element-objects
  '(bold citation citation-reference code entity export-snippet
    footnote-reference inline-babel-call inline-src-block italic
    line-break latex-fragment link macro radio-target statistics-cookie
    strike-through subscript superscript table-cell target timestamp underline verbatim)
  "List of all org-element objects provided by org-element.el in 'org-element-all-objects'")

(defclass org-element ()
  ((kind :initarg :kind :initform nil)))

(defclass org-stream (fundamental-stream)
  ((stream :initarg :stream :reader stream-of)))

(defgeneric org-parse-all (object))

(defclass org-file (org-element org-stream)
  ((path :initarg :path :accessor path)
   (text :initarg :text :accessor text)
   (kind :allocation :class :initform :file)))

(defmethod org-parse-all ((object org-file))
  (let ((lines (sb-unicode:lines (slot-value object 'text))))
  (loop for i from 1 for x in lines
	collect
	(if (cl-ppcre:scan org-headline-regexp x) (list 'headline i x)
	    (if (cl-ppcre:scan org-file-property-regexp x) (list i 'file-property x)
		(if (cl-ppcre:scan org-property-regexp x) (list i 'node-property x)
		    (list i nil x)))))))

(defun read-org-file (path)
  (make-instance 'org-file :path path :text (read-file-string path)))

;; (slot-value (read-org-file "~/org/notes.org") 'text)

(defclass org-slice (org-element)
  ((lines :initarg :lines :initform (vector))
   (kind :allocation :class :initform :slice)))

(defun read-org-slice (&optional stream)
  (make-instance 'org-slice :lines (loop for i in stream collect i)))
