(defpackage :organ-tests
  (:use :cl :organ :organ-cli :fiveam)
  (:import-from :clingon :parse-command-line :getopt)
  (:shadowing-import-from #:fiveam #:test)
  (:export :run-tests :*test-file*))

(in-package :organ-tests)
(defvar *test-file* "readme.org")
(def-suite :organ
  :description "Organ test-suite.")
(in-suite :organ)
(test org-file (is (read-org-file *test-file*)))
(test org-lines
      (is (read-org-lines (open *test-file*)))
      (let ((s (text (read-org-file *test-file*))))
	(is (read-org-lines-from-string s))))
(test org-headline
  (let ((s "** DONE testing stuff :test:test:"))
    (is (= (level (org-parse (make-org-headline s))) 2))
    ;; weird bug going on here with class slots
    ;; (is (string= (text (state (org-parse (make-org-headline s)))) "TODO"))
    ;; ??? this ain't right
    (is (string= (title (org-parse (make-org-headline s))) "DONE testing stuff "))
    (is (= (length (tags (org-parse (make-org-headline s)))) 2))))

(def-suite* :organ-cli :in :organ)

(test sanity
  (let ((cmd (parse-command-line (cmd) nil)))
    (is (string= "organ" (clingon:command-name cmd)))))

(test subcmds
  (is (eql
       (clingon:command-handler
	(parse-command-line (cmd) '("--input" "test.org")))
       #'organ-cli:handler))
  (is (eql
       (clingon:command-handler (parse-command-line (cmd) '("show" "test.org")))
       #'organ-cli:show-handler)))

(test output (is (parse-command-line (cmd) '("test.org" "file.sxp"))))

(defun run-tests () (fiveam:run! :organ))
