(defpackage :organ-cli
  (:use :cl :organ)
  (:import-from
   :clingon
   :make-option
   :make-command
   :getopt
   :run)
  (:export :main :start :opts :handler))

(in-package :organ-cli)

(defun opts ()
  (list
   (make-option
    :flag
    :description "short help"
    :short-name #\h
    :key :help)
   (make-option
    :string
    :description "input"
    :short-name #\i
    :long-name "input"
    :initial-value nil
    :key :input)))

(defun handler (cmd)
  (when (getopt cmd :help)
    (princ "organ [INPUT] [...]")
    (terpri)))

(defun start ()
  (make-command
   :name "organ"
   :description "run organ-cli"
   :options (opts)
   :handler #'handler))

(defun main ()
  (run (start))
  (print "greetings, stranger!")
  (terpri))
