;;; organ.lisp --- Org parser
(defpackage :organ
  (:use :cl :cl-ppcre)
  (:import-from :uiop :read-file-string)
  (:shadowing-import-from :sb-gray :fundamental-stream)
  (:export
   :*org-todo-keyword-types*
   :*org-todo-keywords*
   :org-heading-regexp
   :org-file-property-regexp
   :org-todo-keyword-regexp
   :org-property-regexp
   :org-tag-regexp
   :org-element-types
   :org-element-objects
   :org-file
   :read-org-file
   :read-org-lines
   :org-lines
   :org-stream
   :org-headline
   :make-org-headline
   :org-todo-keyword
   :org-list))

(in-package :organ)

(defparameter *org-todo-keyword-types*
  '(todo wip done))

(defparameter *org-todo-keywords*
  '(("TODO" todo) ("DONE" done) ("FIND" todo) ("FOUND" done)
    ("RESEARCH" todo) ("RECORD" todo) ("OUTLINE" todo) ("DRAFT" todo)
    ("REVIEW" todo) ("FIX" todo) ("IMPL" todo) ("TEST" todo) ("FIXED" done)
    ("GOTO" todo) ("HACK" todo) ("NOTE" todo) ("CODE" todo) ("LINK" todo))
  "List of keywords accepted by `organ'. ")

(defvar org-todo-keyword-map
  (let ((kws (make-hash-table :size 20)))
    (dolist (kw *org-todo-keywords*)
      (let ((k (intern (car kw)))
	    (v (cadr kw)))
	(assert (member v *org-todo-keyword-types*))
	(setf (gethash k kws) v)))
    kws))

(defmacro org-todo-keyword-p (kw)
  "Search for symbol KW in `org-todo-keyword-map' returning the
associated value or nil if not found."
  `(gethash (intern ,kw) org-todo-keyword-map))

(defvar org-headline-regexp (cl-ppcre:parse-string "^([*]+)\\s+(.*)"))
(defvar org-todo-keyword-regexp (cl-ppcre:parse-string "^(\\w+)\\s+(.*)"))
(defvar org-file-property-regexp (cl-ppcre:parse-string "^[#+](.*)[:]\\s+(.*)"))
(defvar org-property-regexp (cl-ppcre:parse-string "^[:](.*)[:]\\s+(.*)"))
;; this doesn't consume leading whitespace. It could be useful in the
;; future to infer a value for org-tags-column but is contained in the
;; title slot of `org-headline' for now. The result of this scan is a
;; single string delimited by the ':' character. To get a list of tags
;; as strings, use `org-tag-split'.
(defvar org-tag-regexp (cl-ppcre:parse-string "(:[\\w_@#%:]+:)"))

(defun org-tag-split (tags)
  (remove-if (lambda (s) (typep s '(string 0))) (cl-ppcre:split ":" tags)))

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

(defgeneric org-parse (self)
  (:documentation "Parse the text slot from ORG-ELEMENT."))

(defgeneric org-parse-lines (self)
  (:documentation "Parse the text slot from ORG-ELEMENT as a vector of lines."))

(defmacro org-init (class &optional text)
  "Initialize a instance of `org-element' CLASS with optional TEXT."
  `(make-instance ',class ,@(when text `(:text ,text))))

;; parent and children are implicit. A single instance of
;; `org-element' contains a complete org-mode AST.
(defclass org-element ()
  ((text :initarg :text :accessor text :type string)
   (kind :initarg :kind :type keyword)))

(defmethod org-parse-lines ((self org-element))
  (let ((lines (lines (read-org-lines-from-string (slot-value self 'text)))))
  (loop for i from 1 for x across lines
	collect
	(if (cl-ppcre:scan org-headline-regexp x) (list i 'headline x)
	    (if (cl-ppcre:scan org-file-property-regexp x) (list i 'file-property x)
		(if (cl-ppcre:scan org-property-regexp x) (list i 'node-property x)
		    (list i nil x)))))))

(defclass org-stream (fundamental-stream)
  ((stream :initarg :stream :reader stream-of)))

(defclass org-file (org-element org-stream)
  ((path :initarg :path :accessor path)
   (kind :allocation :class :initform :file)))

(defun read-org-file (path)
  (make-instance 'org-file :path path :text (read-file-string path)))

;; (slot-value (read-org-file "~/org/notes.org") 'text)

(defclass org-lines (org-element)
  ((lines :initarg :lines :type vector :accessor lines)
   (kind :allocation :class :initform :org-lines)))

(defun read-org-lines (&optional stream)
  (let ((slice (make-instance 'org-lines)))
    (setf (lines slice)
	  (apply #'vector
		 (loop for l = (read-line stream nil :eof)
		       until (eq l :eof)
		       collect l)))
    slice))

(defun read-org-lines-from-string (str)
  (with-input-from-string (s str) (read-org-lines s)))

;; when level=0, headline is uninitialized
(defclass org-headline (org-element)
  ((kind :allocation :class :initform :org-headline)
   (state :accessor state :initform nil)
   (level :accessor level :initform 0)
   (props :accessor props :initform nil)
   (priority :accessor priority :initform nil)
   (tags :accessor tags :initform nil)
   (title :accessor title :initform "")))

(defun make-org-headline (text) (org-init org-headline text))
(defmethod org-parse ((self org-headline))
  (with-input-from-string (s (text self))
      (when (peek-char #\* s)
	(let ((line (read-line s)))
	  (multiple-value-bind (start _ reg-start reg-end)
	      ;; scan for headline
	      (cl-ppcre:scan org-headline-regexp line)
	    (declare (ignore _))
	      (when start
		(loop for rs across reg-start
		      for re across reg-end
		      for i from 0
		      do
			 (if (= i 0)
			     (setf (level self) (- re rs))
			     (let ((sub (subseq line rs)))
			       (multiple-value-bind (match subs)
				   ;; scan for todo-keyword
				   (cl-ppcre:scan-to-strings org-todo-keyword-regexp sub)
				 (declare (ignorable subs))
				 (if match
				     (let ((kw? (svref subs 0)))
				       (if (and kw? (org-todo-keyword-p kw?))
					   (progn
					     (setf (state self) (make-org-todo-keyword kw?))
					     (setf (title self) (svref subs 1)))
					   (setf (title self) match)))
				     (setf (title self) sub))))))))
	  ;; scan for tags, modifies title slot
	  (let ((start (cl-ppcre:scan org-tag-regexp (title self))))
	    (when start
	      (setf (tags self) (apply #'vector (mapcar #'make-org-tag (org-tag-split (subseq (title self) start)))))
	      (setf (title self) (subseq (title self) 0 start)))))
	;; TODO 2023-07-24: cookies,priority
	self)))

(defclass org-todo-keyword (org-element)
  ((kind :allocation :class :initform :org-todo-keyword)
   (todo-type :accessor todo-type :initform nil :type symbol)))

(defun make-org-todo-keyword (text) (org-parse (org-init org-todo-keyword text)))

(defmethod org-parse ((self org-todo-keyword))
  (let* ((text (slot-value self 'text))
	 (type (gethash (intern text) org-todo-keyword-map nil)))
    (if type (setf (slot-value self 'todo-type) type))
    self))

(defclass org-list (org-element)
  ((kind :allocation :class :initform :org-list)))

(defclass org-tag (org-element)
  ((kind :allocation :class :initform :org-tag)))

(defun make-org-tag (text) (org-parse (org-init org-tag text)))

(defmethod org-parse ((self org-tag)) self) ;; nop

(defclass org-block (org-element) ())

(defclass org-paragraph (org-element) ())
