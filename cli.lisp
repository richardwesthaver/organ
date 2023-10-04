(defpackage :organ.cli
  (:use :cl :organ :macs :cli :sym :log :fu :fmt)
  (:export :main :+cli+))

(in-package :organ.cli)

(define-cli :constant +cli+
    (make-cli t :name "organ"
		:version "0.0.1"
		:description "organ.cli"
		:opts (make-opts 
			(:name debug :global t)
			(:name help :global t)
			(:name version :global t))
		:cmds (make-cmds 
			(:name inspect :opts (make-opts (:name input)) :thunk '(inspect (read-org-file $a1)))
			(:name show :thunk '(fmt-tree t (remove-if-not (lambda (x) (eql (cadr x) 'headline)) 
							 (org-parse-lines (read-org-file (open $a1)))) 
					     :layout :down))
			(:name parse :opts (make-opts (:name input) (:name output))))))

(defun run ()
  (with-cli (opts cmds args) +cli+
    (when (cli-val (find-opt +cli+ "debug")) (setq *log-level* :debug))
    (debug! (cli-opts +cli+) (cli-cmd-args +cli+) (cli-cmds +cli+))
    (when-let ((a (cli-cmd-args (find-cmd +cli+ "inspect"))))
      (inspect (read-org-file (open (car a)))))
    (when-let ((a (cli-cmd-args (find-cmd +cli+ "parse"))))
      (fmt-tree t (remove-if #'null (org-parse-lines (read-org-file (open (car a))))) :layout :down))
    (when-let ((a (cli-cmd-args (find-cmd +cli+ "show"))))
      (fmt-tree t 
		(mapcar (lambda (x) `(,(car x) ,(cddr x)))
			(remove-if-not (lambda (x) (equal (cadr x) (symb 'headline))) 
				       (org-parse-lines (read-org-file (open (car a))))))
		:layout :down))
    (when (cli-val (find-opt +cli+ "help")) (print-help +cli+))
    (when (cli-val (find-opt +cli+ "version")) (print-version +cli+))))

(defmain ()
  (run)
  (sb-ext:exit :code 0))
