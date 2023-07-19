(defpackage :organ-cli
  (:use :cl :organ :clingon)
  (:import-from
   :clingon
   :make-option
   :make-command
   :command-arguments
   :getopt
   :run)
  (:export :main :start :handler :props-handler :props-cmd :cmd :cmds :mk-slot :mk-short :opt :opts))

(in-package :organ-cli)

;; we define some utils as macros so that we can use them at
;; compile-time.
(defmacro mk-slot (name) "make slot-name"
  `(intern ,(string-upcase name) :keyword))

(defmacro mk-short (name) "make short-name"
  `(character (aref (if (stringp ,name) ,name (symbol-name ,name)) 0)))

(defmacro opt (name desc &optional init type)
  "shorthand for `make-option'."
  `(make-option
    ,(if type (mk-slot type) ':string)
    :description ,desc
    :short-name ,(mk-short name)
    :long-name ,name
    :initial-value ,(or init)
    :key ,(mk-slot name)))

(defun opts ()
  (list (opt "input" "input file")
	(opt "output" "output file")
	(opt "config" "sxp config")))
  
(defun props-handler (cmd)
  (let ((args (command-arguments cmd)))
    (format t "running props on ~A~%" args)))

(defun props-cmd ()
  (make-command :name "props"
		:description "print a list of file properties found in INPUT"
		:handler #'props-handler))

(defun cmds ()
  (list (props-cmd)))

(defun handler (cmd)
  (let ((args (command-arguments cmd)))
    (format t "~A~%" args)))

(defun cmd ()
  (make-command :name "organ"
		:version "0.1.0"
		:description "organ-cli"
		:options (opts)
		:sub-commands (cmds)
		:handler #'handler))

(defun main () (run (cmd)))
