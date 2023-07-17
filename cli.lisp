(defpackage :organ-cli
  (:use :cl :organ)
  (:import-from
   :clingon
   :make-option
   :make-command
   :getopt
   :run)
  (:export :main :start :handler :*organ-cmd*))

(in-package :organ-cli)

(defmacro mk-slot (name) "make slot-name"
  `(intern (string-upcase (if (stringp ,name) ,name ,(string name))) :keyword))

(defmacro mk-short (name) "make short-name"
  `(character (aref (if (stringp ,name) ,name (symbol-name ,name)) 0)))

(defmacro opt (name desc &optional init type)
  "shorthand for `make-option'."
  `(make-option
    ,(if type (mk-slot type) ':string)
    :description ,desc
    :short-name ,(mk-short name)
    :long-name ,(string name)
    :initial-value ,(or init)
    :key ,(mk-slot name)))

(defun handler (cmd)
  (when (getopt cmd :help)
    (princ "organ [INPUT] [...]")
    (terpri)))

(defparameter *organ-cmd*
  (make-command :name "organ"
		:description "run organ-cli"
		:options
		(list
		 (opt "input" "input file")
		 (opt "output" "output file")
		 (opt "config" "sxp config"))
		:handler #'handler))

(defun main ()
  (run *organ-cmd*)
  (print "OK.")
  (terpri))
