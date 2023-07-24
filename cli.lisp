(defpackage :organ-cli
  (:use :cl :organ :clingon)
  (:import-from
   :clingon
   :make-option
   :make-command
   :command-arguments
   :getopt
   :run)
  (:export :main :start :handler :show-handler :show-cmd :parse-handler :parse-cmd :cmd :cmds :mk-slot :mk-short :opt :opts))

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
  
(defun show-handler (cmd)
  (let ((args (command-arguments cmd)))
    (format t "running show ~A~%" args)))

(defun show-cmd ()
  (make-command :name "show"
		:description "show details"
		:handler #'show-handler))

(defun parse-handler (cmd)
  (let ((args (command-arguments cmd)))
    (format t "running parser on ~A~%" args)))

(defun parse-cmd ()
  (make-command :name "parse"
		:description "print org-ast"
		:handler #'parse-handler))

(defun cmds ()
  (list
   (show-cmd)
   (parse-cmd)))

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
