(defpackage :organ-cli
  (:use :cl :organ :clingon)
  (:export :main))

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

(defun start-cli ()
  (make-command
   :name "organ"
   :description "run organ-cli"
   :options (opts)
   :handler #'handler))

(defun main ()
  (run (start-cli))
  (print "greetings, stranger!")
  (terpri))
