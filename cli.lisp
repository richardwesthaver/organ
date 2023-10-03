(defpackage :organ.cli
  (:use :cl :organ :macs :cli :sym :log)
  (:export :*opts* :*cmds* :*cli* :main))

(in-package :organ.cli)

(defvar *opts* (make-opts 
		 (:name debug :global t)
		 (:name help :global t)
		 (:name version :global t)))

(defvar *cmds* (make-cmds 
		 (:name show :opts (make-opts (:name input)))
		 (:name parse :opts (make-opts (:name input) (:name output)))))

(defvar *cli* 
  (make-cli t :name "organ"
	      :version "0.0.1"
	      :description "organ.cli"
	      :opts *opts*
	      :cmds *cmds*))

(defun run ()
  (with-cli (opts cmds args) *cli*
    (when (cli-val (find-opt *cli* "debug")) (setq *log-level* :debug))
    (when (cli-val (find-opt *cli* "help")) (print-help *cli*))
    (when (cli-val (find-opt *cli* "version")) (print-version *cli*))
    (debug! (cli-opts *cli*) (cli-cmd-args *cli*) (cli-cmds *cli*))
    (when-let ((a (cli-cmd-args (find-cmd *cli* "show"))))
      (print a))))

(defmain ()
  (run)
  (sb-ext:exit :code 0))
