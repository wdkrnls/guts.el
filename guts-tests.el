;;; guts-tests.el --- ERT tests for guts-analyze-expression  -*- lexical-binding: t; -*-

;;; Commentary:
;; Load the analysis code first, then either M-x ert-run-tests-interactively,
;; or:  emacs -batch -l guts-analyze -l guts-analyze-tests -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'seq)

;;;; Test helpers -------------------------------------------------------

(defun guts-test--collect (text)
  "Return (CALLS . VARS), sorted and de-duped, collected from TEXT."
  (let ((guts--calls nil) (guts--vars nil)
        (rest text) (forms nil))
    (condition-case nil
        (while t
          (let ((res (read-from-string rest)))
            (push (car res) forms)
            (setq rest (substring rest (cdr res)))))
      (end-of-file nil))
    (dolist (form (nreverse forms))
      (guts-collect-sexp-symbols form))
    (cons (sort (delete-dups guts--calls) #'string<)
          (sort (delete-dups guts--vars) #'string<))))

(defun guts-test--calls (text) (car (guts-test--collect text)))
(defun guts-test--vars (text)  (cdr (guts-test--collect text)))

(defun guts-test--section (report title)
  "Return the blank-line-delimited section of REPORT mentioning TITLE."
  (seq-find (lambda (s) (string-match-p title s))
            (split-string report "\n\n")))

;;;; Collector: basic structure -----------------------------------------

(ert-deftest guts-test--collect-simple-call ()
  (should (equal (guts-test--calls "(foo a b)") '(foo)))
  (should (equal (guts-test--vars "(foo a b)") '(a b))))

(ert-deftest guts-test--collect-nested-calls ()
  (should (equal (guts-test--calls "(foo (bar (baz)))") '(bar baz foo))))

(ert-deftest guts-test--collect-multiple-forms ()
  (should (equal (guts-test--calls "(foo) (bar)") '(bar foo))))

(ert-deftest guts-test--collect-vector-elements-are-vars ()
  (should (equal (guts-test--vars "[foo bar]") '(bar foo)))
  (should (equal (guts-test--calls "[foo bar]") nil)))

;;;; Collector: quote / function ---------------------------------------

(ert-deftest guts-test--collect-quote-is-data ()
  (should (equal (guts-test--calls "(quote (foo bar))") nil))
  (should (equal (guts-test--vars "(quote (foo bar))") nil)))

(ert-deftest guts-test--collect-function-symbol ()
  (should (equal (guts-test--calls "(function foo)") '(foo))))

;;;; Collector: lambda and let -----------------------------------------

(ert-deftest guts-test--collect-lambda-arglist-skipped ()
  (should (equal (guts-test--calls "(lambda (x) (foo x))") '(foo)))
  (should (equal (guts-test--vars "(lambda (x) (foo x))") '(x))))

(ert-deftest guts-test--collect-let-binding-names-skipped ()
  (should (equal (guts-test--calls "(let ((x (foo))) (bar x))") '(bar foo)))
  (should (equal (guts-test--vars "(let ((x (foo))) (bar x))") '(x))))

(ert-deftest guts-test--collect-let*-binding-names-skipped ()
  (should (equal (guts-test--calls "(let* ((x (foo)) (y (baz))) (bar x y))")
                 '(bar baz foo)))
  (should (equal (guts-test--vars "(let* ((x (foo)) (y (baz))) (bar x y))")
                 '(x y))))

;;;; Collector: defining forms -----------------------------------------

(ert-deftest guts-test--collect-defvar-name-skipped ()
  (should (equal (guts-test--calls "(defvar foo 42)") nil))
  (should (equal (guts-test--vars "(defvar foo 42)") nil)))

(ert-deftest guts-test--collect-defvar-value-walked ()
  (should (equal (guts-test--calls "(defvar foo (bar))") '(bar))))

(ert-deftest guts-test--collect-defun ()
  (should (equal (guts-test--calls "(defun foo (x) (bar x))") '(bar)))
  (should (equal (guts-test--vars "(defun foo (x) (bar x))") '(x))))

(ert-deftest guts-test--collect-defcustom ()
  (should (equal (guts-test--calls "(defcustom foo 1 \"doc\")") nil))
  (should (equal (guts-test--vars "(defcustom foo 1 \"doc\")") nil)))

;;;; Collector: backquote templates ------------------------------------

(ert-deftest guts-test--backquote-symbols-are-data ()
  (should (equal (guts-test--calls "`(foo bar)") (sort '(\`) #'string<)))
  (should (equal (guts-test--vars "`(foo bar)") nil)))

(ert-deftest guts-test--backquote-unquote-payload-is-code ()
  (should (equal (guts-test--calls "`(foo ,(bar baz))")
                 (sort '(\` \, bar) #'string<)))
  (should (equal (guts-test--vars "`(foo ,(bar baz))") '(baz))))

(ert-deftest guts-test--backquote-unquote-splicing-payload-is-code ()
  (should (equal (guts-test--calls "`(,@items)") (sort '(\` \,@) #'string<)))
  (should (equal (guts-test--vars "`(,@items)") '(items))))

(ert-deftest guts-test--backquote-nested ()
  (should (equal (guts-test--calls "`(a `(b ,c))") (sort '(\` \,) #'string<)))
  (should (equal (guts-test--vars "`(a `(b ,c))") '(c))))

;;;; Categorisation ----------------------------------------------------

(ert-deftest guts-test--reader-syntax-p ()
  (should (guts--reader-syntax-p '\`))
  (should (guts--reader-syntax-p '\,))
  (should (guts--reader-syntax-p '\,@))
  (should-not (guts--reader-syntax-p 'car))
  (should-not (guts--reader-syntax-p 'if)))

(ert-deftest guts-test--reader-symbols-are-exclusive ()
  (dolist (sym '(\` \, \,@))
    (should-not (guts--special-form-p sym))
    (should-not (guts--macro-p sym))
    (should-not (guts--plain-function-p sym))))

(ert-deftest guts-test--special-form-p ()
  (should (guts--special-form-p 'if))
  (should (guts--special-form-p 'let))
  (should-not (guts--special-form-p 'car))
  (should-not (guts--special-form-p '\`)))

(ert-deftest guts-test--macro-p ()
  (should (guts--macro-p 'when))
  (should-not (guts--macro-p 'car))
  (should-not (guts--macro-p 'if))
  (should-not (guts--macro-p '\`)))

(ert-deftest guts-test--plain-function-p ()
  (should (guts--plain-function-p 'car))
  (should-not (guts--plain-function-p 'when))
  (should-not (guts--plain-function-p 'if))
  (should-not (guts--plain-function-p '\`)))

;;;; Report formatting -------------------------------------------------

(ert-deftest guts-test--report-section-splits ()
  (let ((out (guts--report-section "Functions" '(car cdr) '(car))))
    (should (string-match-p "Functions (2)" out))
    (should (string-match-p "local: +car" out))
    (should (string-match-p "external: cdr" out))))

(ert-deftest guts-test--report-section-no-local ()
  (let ((out (guts--report-section "Reader syntax" '(\` \,) nil t)))
    (should (string-match-p "Reader syntax (2)" out))
    (should-not (string-match-p "local:" out))
    (should-not (string-match-p "external:" out))))

(ert-deftest guts-test--report-section-empty ()
  (let ((out (guts--report-section "Macros" nil nil)))
    (should (string-match-p "Macros (0)" out))
    (should (string-match-p "local: +(none)" out))
    (should (string-match-p "external: (none)" out))))

(ert-deftest guts-test--format-report-sections ()
  (let ((out (guts--format-report nil nil '(\` if when car) nil)))
    (should (string-match-p "Reader syntax (1)" out))
    (should (string-match-p "Special forms (1)" out))
    (should (string-match-p "Macros (1)" out))
    (should (string-match-p "Functions (1)" out))
    (should (string-match-p "Variables (0)" out))
    (should (string-match-p "Custom variables (0)" out))))

(ert-deftest guts-test--format-report-suppresses-impossible-splits ()
  (let* ((out (guts--format-report nil nil '(\` if) nil))
         (reader (guts-test--section out "Reader syntax"))
         (special (guts-test--section out "Special forms")))
    (should-not (string-match-p "local:" reader))
    (should-not (string-match-p "external:" reader))
    (should-not (string-match-p "local:" special))
    (should-not (string-match-p "external:" special))))

(ert-deftest guts-test--format-report-keeps-split-for-macros ()
  (let* ((out (guts--format-report nil nil '(when) '(when)))
         (macros (guts-test--section out "Macros")))
    (should (string-match-p "local: +when" macros))))

;;;; Integration through guts-analyze-string -----------------------------

(ert-deftest guts-test--analyze-string-calls-and-globals ()
  (pcase-let ((`(,globals ,customs ,calls)
               (guts-analyze-string "(list load-path (car x))")))
    (should (equal calls '(car list)))
    (should (memq 'load-path globals))
    (should-not (memq 'x globals))      ; x is unbound, so filtered out
    (should (equal customs nil))))

(ert-deftest guts-test--analyze-string-custom ()
  (require 'cus-edit)
  (pcase-let ((`(,_ ,customs ,_) (guts-analyze-string "(list custom-file)")))
    (should (memq 'custom-file customs))))

;;;; Buffer / point helpers --------------------------------------------

(ert-deftest guts-test--expression-bounds-enclosing ()
  (with-temp-buffer
    (insert "(defun foo ()\n  (bar (baz)))\n")
    (goto-char (point-min))
    (search-forward "baz")
    (let ((bounds (guts--expression-bounds)))
      (should (equal (buffer-substring (car bounds) (cdr bounds))
                     "(defun foo ()\n  (bar (baz)))")))))

(ert-deftest guts-test--expression-bounds-on-paren ()
  (with-temp-buffer
    (insert "(foo (bar))")
    (goto-char (point-min))
    (let ((bounds (guts--expression-bounds)))
      (should (equal (buffer-substring (car bounds) (cdr bounds))
                     "(foo (bar))")))))

(ert-deftest guts-test--expression-bounds-atom ()
  (with-temp-buffer
    (insert "foo bar")
    (goto-char (point-min))
    (search-forward "foo")
    (should-not (guts--expression-bounds))))

(ert-deftest guts-test--expression-bounds-string-error ()
  (with-temp-buffer
    (insert "(foo \"bar\")")
    (goto-char (point-min))
    (search-forward "bar")
    (should-error (guts--expression-bounds) :type 'user-error)))

(ert-deftest guts-test--expression-bounds-comment-error ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo) ; comment\n")
    (goto-char (point-min))
    (search-forward "comment")
    (should-error (guts--expression-bounds) :type 'user-error)))

(ert-deftest guts-test--buffer-definitions ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defvar foo 1)\n(defun bar () nil)\n"
            "(defcustom baz 2 \"d\")\n(defmacro qux () nil)\n(foo bar)\n")
    (should (equal (sort (guts--buffer-definitions) #'string<)
                   '(bar baz foo qux)))))

(ert-deftest guts-test--definition-heads-membership ()
  (dolist (h '(defvar defconst defcustom defun defmacro defsubst defalias))
    (should (memq h guts--definition-heads))))

(provide 'guts-analyze-tests)
;;; guts-tests.el ends here
