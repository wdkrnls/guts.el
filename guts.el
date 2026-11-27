;; -*- lexical-binding: t; -*-

(defvar guts--reader-symbols '(\` \, \,@)
  "Symbols introduced by reader syntax rather than by real calls.
These are the reader forms for backquote, unquote and
unquote-splicing; they carry no function binding of their own.")

(defun guts--reader-syntax-p (sym)
  "Non-nil if SYM is a reader-syntax marker."
  (memq sym guts--reader-symbols))

(defun guts--special-form-p (sym)
  "Non-nil if SYM is an Emacs special form."
  (and (not (guts--reader-syntax-p sym))
       (if (fboundp 'special-form-p)
           (special-form-p sym)
         (memq sym guts--special-forms-fallback))))

(defun guts--macro-p (sym)
  "Non-nil if SYM names a macro."
  (and (not (guts--reader-syntax-p sym))
       (fboundp sym)
       (macrop sym)))

(defun guts--plain-function-p (sym)
  "Non-nil if SYM names an ordinary (non-macro, non-special-form) function."
  (and (not (guts--reader-syntax-p sym))
       (fboundp sym)
       (not (guts--special-form-p sym))
       (not (guts--macro-p sym))))

(defun guts--format-report (globals customs calls local)
  "Format the analysis report; LOCAL is the set of names defined here."
  (concat
   (guts--report-section "Reader syntax" (seq-filter #'guts--reader-syntax-p calls) local t)
   "\n"
   (guts--report-section "Special forms" (seq-filter #'guts--special-form-p calls) local t)
   "\n"
   (guts--report-section "Macros" (seq-filter #'guts--macro-p calls) local)
   "\n"
   (guts--report-section "Functions" (seq-filter #'guts--plain-function-p calls) local)
   "\n"
   (guts--report-section "Variables" globals local)
   "\n"
   (guts--report-section "Custom variables" customs local)))
(defvar guts--calls nil
  "Accumulator of called symbols for `guts-collect-sexp-symbols'.")
(defvar guts--vars nil
  "Accumulator of free variables for `guts-collect-sexp-symbols'.")

(defun guts-collect-template (form)
  "Walk FORM as a backquote template.
Symbols inside the template are data, not calls, but the payloads
of unquote and unquote-splicing are walked as ordinary code."
  (cond
   ((consp form)
    (let ((head (car form)))
      (cond
       ((eq head '\`)
        (push head guts--calls)            ; still reported as reader syntax
        (guts-collect-template (cadr form)))
       ((memq head '(\, \,@))
        (push head guts--calls)
        (guts-collect-body (cdr form)))    ; payload is evaluated code
       (t
        (guts-collect-template head)
        (guts-collect-template (cdr form))))))
   ((vectorp form)
    (mapc #'guts-collect-template form))
   (t nil)))

(defun guts-collect-body (body)
  "Walk each form in the list BODY as a single form."
  (dolist (form body)
    (guts-collect-sexp-symbols form)))

(defun guts-collect-sexp-symbols (form)
  "Walk FORM, pushing called symbols onto `guts--calls' and free
variables onto `guts--vars'."
  (cond
   ((symbolp form)
    (unless (memq form '(nil t)) (push form guts--vars)))
   ((consp form)
    (let ((head (car form)))
      (cond
       ((eq head 'quote) nil)
       ((eq head 'function)
        (let ((arg (cadr form)))
          (if (consp arg)
              (guts-collect-body (cddr form))
            (when (symbolp arg) (push arg guts--calls)))))
       ((memq head '(lambda closure))
        (guts-collect-body (cddr form)))
       ((memq head '(let let*))
        (dolist (binding (cadr form))
          (when (consp binding) (guts-collect-body (cdr binding))))
        (guts-collect-body (cddr form)))
       ((memq head '(defvar defconst defcustom defgroup defface defalias))
        (guts-collect-body (cddr form)))
       ((memq head '(defun defmacro defsubst))
        (guts-collect-body (cdddr form)))
       ((eq head '\`)
        (guts-collect-template form))
       (t
        (if (symbolp head)
            (push head guts--calls)
          (guts-collect-sexp-symbols head))
        (guts-collect-body (cdr form))))))
   ((vectorp form)
    (mapc #'guts-collect-sexp-symbols form))))

(defun guts-analyze-string (text)
  "Return (GLOBALS CUSTOMS CALLS) for the forms read from TEXT."
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
    (let ((globals (seq-filter (lambda (s) (and (not (keywordp s)) (boundp s)))
                               (delete-dups guts--vars))))
      (list (sort globals #'string<)
            (seq-filter #'custom-variable-p globals)
            (sort (seq-filter (lambda (s) (and (not (keywordp s)) (fboundp s)))
                              (delete-dups guts--calls))
                  #'string<)))))

;;;; Where to read from --------------------------------------------------

(defun guts--expression-bounds ()
  "Return (BEG . END) for the outermost list enclosing point, or nil."
  (let ((ppss (syntax-ppss)))
    (cond
     ((or (nth 3 ppss) (nth 4 ppss))
      (user-error "Point is inside a string or comment"))
     ((> (car ppss) 0)
      (save-excursion
        (backward-up-list (car ppss))
        (let ((beg (point)))
          (forward-list)
          (cons beg (point)))))
     ((looking-at "\\s(")
      (save-excursion
        (let ((beg (point)))
          (forward-list)
          (cons beg (point)))))
     (t nil))))

;;;; Categorisation ------------------------------------------------------

(defvar guts--special-forms-fallback
  '(if while let let* progn prog1 prog2 cond and or catch throw
    unwind-protect save-excursion save-restriction save-current-buffer
    setq setq-default quote function lambda condition-case interactive)
  "Special forms used only if `special-form-p' is unavailable.")

(defvar guts--definition-heads
  '(defvar defvar-local defconst defcustom defgroup defface defvaralias
    defalias defun defsubst defmacro
    cl-defun cl-defsubst cl-defmacro cl-defgeneric
    define-minor-mode define-globalized-minor-mode
    define-derived-mode define-generic-mode define-skeleton)
  "Form heads whose second element names something defined by that form.")

(defun guts--buffer-definitions ()
  "Return the symbols defined by top-level forms in the current buffer."
  (let (names)
    (save-excursion
      (goto-char (point-min))
      (condition-case nil
          (while (progn (forward-comment (point-max))
                        (not (eobp)))
            (let ((form (read (current-buffer))))
              (when (and (consp form)
                         (memq (car form) guts--definition-heads)
                         (symbolp (cadr form)))
                (push (cadr form) names))))
        (error nil)))
    (nreverse names)))

(defun guts--report-section (title syms local &optional no-local)
  "Format a section named TITLE for SYMS, split by membership in LOCAL.
When NO-LOCAL is non-nil the local/external split is omitted."
  (if no-local
      (format "%s (%d)\n  %s\n"
              title (length syms)
              (if syms (mapconcat #'symbol-name (sort syms #'string<) " ") "(none)"))
    (let (loc ext)
      (dolist (s syms)
        (if (memq s local) (push s loc) (push s ext)))
      (format "%s (%d)\n  local:    %s\n  external: %s\n"
              title (+ (length loc) (length ext))
              (if loc (mapconcat #'symbol-name (sort loc #'string<) " ") "(none)")
              (if ext (mapconcat #'symbol-name (sort ext #'string<) " ") "(none)")))))

;;;; Command -------------------------------------------------------------

(defun guts-analyze-expression (&optional beg end)
  "Report calls and variables used by the expression at point.
With an active region, restrict the search to that region."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let ((bounds (guts--expression-bounds)))
       (unless bounds (user-error "No enclosing expression at point"))
       (list (car bounds) (cdr bounds)))))
  (pcase-let* ((text (buffer-substring-no-properties beg end))
               (`(,globals ,customs ,calls) (guts-analyze-string text))
               (local (guts--buffer-definitions)))
    (with-output-to-temp-buffer "*guts sexp analysis*"
      (princ (guts--format-report globals customs calls local)))))
