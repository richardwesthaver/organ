(defpackage :organ.cli
  (:use :cl :organ :macs :cli :sym :log :fu :fmt :ana)
  (:export :main :$cli))

(in-package :organ.cli)

(define-cli $cli
  :name "organ"
  :version "0.0.1"
  :description "organ.cli"
  :opts (make-opts 
	  (:name debug :global t)
	  (:name help :global t)
	  (:name version :global t))
  :cmds (make-cmds 
	  (:name inspect :opts (make-opts (:name input)) :thunk (lambda (a) (inspect (read-org-file a))))
	  (:name show :thunk (lambda (a) (fmt-tree t (remove-if-not (lambda (x) (eql (cadr x) 'headline)) 
								    (org-parse-lines (read-org-file (open a))))
						   :layout :down)))
	  (:name parse 
	   :opts (make-opts (:name input) (:name output))
	   :thunk nil))
  (lambda (x y z) (* x y z)))

(defun run ()
  (with-cli (opts cmds args) $cli
    (when (find-opt $cli "debug" t) (setq *log-level* :debug))
    (debug! (cli-opts $cli) (cli-cmd-args $cli) (cli-cmds $cli))

    (when-let ((a (find-cmd $cli "inspect" t)))
      (inspect (read-org-file (open (car a)))))
      
    (when-let ((a (find-cmd $cli "parse" t)))
      (fmt-tree t (remove-if #'null (org-parse-lines (read-org-file (open (car a))))) :layout :down))
      
    (when-let ((a (find-cmd $cli "show" t)))
      (fmt-tree t 
		(mapcar (lambda (x) `(,(car x) ,(cddr x)))
			(remove-if-not (lambda (x) (equal (cadr x) (symb 'headline))) 
				       (org-parse-lines (read-org-file (open (car a))))))
		:layout :down))

    (when (find-opt $cli "help" t) (print-help $cli))
    (when (find-opt $cli "version" t) (print-version $cli))))

(defmain ()
  (run)
  (sb-ext:exit :code 0))
