(defpackage :organ.cli
  (:use :cl :organ :macs.sym :macs.cli :clingon)
  (:export :main :start :handler :show/handler :show/cmd :parse/handler :parse/cmd :cmd :cmds :opt :opts))

(in-package :organ.cli)

(defmacro opt (name desc &optional init type persist)
  "shorthand for `make-option'."
  `(make-option
    ,(if type (make-slot-name type) ':string)
    :description ,desc
    :short-name ,(make-short-name name)
    :long-name ,name
    :initial-value ,(or init)
    ,@(when persist '(:persistent t))
    :key ,(make-slot-name name)))

(defun opts ()
  (list (opt "input" "input file" nil nil t)
	(opt "output" "output file" nil nil t)
	(opt "config" "sxp config" nil nil t)))

(defun show/opts ()
  `(,(opt "what" "specify what to show")))

(defun show/handler (cmd)
  (let ((args (command-arguments cmd)))
    (print (format t "show ~A~%" args))))

(defun show/cmd ()
  (make-command :name "show"
		:description "show details"
		:options (show/opts)
		:handler #'show/handler))

(defun parse/handler (cmd)
  (let ((args (command-arguments cmd)))
    (format t "running parser on ~A~%" args)))

(defun parse/cmd ()
  (make-command :name "parse"
		:description "print org-ast"
		:handler #'parse/handler))

(defun cmds ()
  (list
   (show/cmd)
   (parse/cmd)))

(defun handler (cmd)
  (format t "got input: ~s~%" (getopt cmd :input))
  (format t "~A~%" args))

(defun cmd ()
  (make-command :name "organ"
		:version "0.1.0"
		:description "organ.cli"
		:options (opts)
		:sub-commands (cmds)
		:handler #'handler))

(defun main () (run (cmd)))
