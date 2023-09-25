(defpackage :organ.cli
  (:use :cl :organ :macs.sym :macs.cli)
  (:export :*opts* :*cmds* :*cli* :main))

(in-package :organ.cli)

(defvar *opts* (make-opts '(:name version)
			  '(:name log :global t)
			  '(:name input)
			  '(:name output)
			  '(:name config)))

(defvar *cmds* (make-cmds '(:name show)
			  '(:name parse)))

(defvar *cli* 
  (make-cli t :name "organ"
	      :version "0.0.1"
	      :description "organ.cli"
	      :opts *opts*
	      :cmds *cmds*))

(defun run ()
  (with-cli (opts cmds) *cli*
    (print-help *cli*)))

(defmain ()
  (run)
  (sb-ext:exit :code 0))
