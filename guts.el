(defvar my--calls nil
  "Accumulator of called symbols for `my-collect-sexp-symbols'.")

(defvar my--vars nil
  "Accumulator of free variables for `my-collect-sexp-symbols'.")

(defun my--expression-bounds ()
  "Return (BEG . END) for the outermost list enclosing point, or nil."
  (let ((ppss (syntax-ppss)))
    (cond
     ((or (nth 3 ppss) (nth 4 ppss))        ; in a string or comment
      (user-error "Point is inside a string or comment"))
     ((> (car ppss) 0)                      ; (car ppss) = paren depth
      (save-excursion
        (backward-up-list (car ppss))       ; climb out exactly depth levels
        (let ((beg (point)))
          (forward-list)
          (cons beg (point)))))
     ((looking-at "\\s(")                   ; point sits on an opening paren
      (save-excursion
        (let ((beg (point)))
          (forward-list)
          (cons beg (point)))))
     (t nil))))

(defun my-collect-body (body)
  (dolist (form body)
    (my-collect-sexp-symbols form)))

(defun my-collect-sexp-symbols (form)
  (cond
   ((symbolp form)
    (unless (memq form '(nil t)) (push form my--vars)))
   ((consp form)
    (let ((head (car form)))
      (cond
       ((eq head 'quote) nil)
       ((eq head 'function)
        (let ((arg (cadr form)))
          (if (consp arg)
              (my-collect-body (cddr form))
            (when (symbolp arg) (push arg my--calls)))))
       ((memq head '(lambda closure))
        (my-collect-body (cddr form)))
       ((memq head '(let let*))
        (dolist (binding (cadr form))
          (when (consp binding) (my-collect-body (cdr binding))))
        (my-collect-body (cddr form)))
       ((memq head '(defvar defconst defcustom defgroup defface defalias))
        (my-collect-body (cddr form)))
       ((memq head '(defun defmacro defsubst))
        (my-collect-body (cdddr form)))
       (t
        (if (symbolp head)
            (push head my--calls)
          (my-collect-sexp-symbols head))
        (my-collect-body (cdr form))))))
   ((vectorp form)
    (my-collect-body form))))

(defun my-analyze-string (text)
  "Return (GLOBALS CUSTOMS PROCS) for the forms read from TEXT."
  (let ((my--calls nil) (my--vars nil)
        (rest text) (forms nil))
    (condition-case nil
        (while t
          (let ((res (read-from-string rest)))
            (push (car res) forms)
            (setq rest (substring rest (cdr res)))))
      (end-of-file nil))
    (dolist (form (nreverse forms))
      (my-collect-sexp-symbols form))
    (let ((globals (seq-filter (lambda (s) (and (not (keywordp s)) (boundp s)))
                               (delete-dups my--vars))))
      (list (sort globals #'string<)
            (seq-filter #'custom-variable-p globals)
            (sort (seq-filter (lambda (s) (and (not (keywordp s)) (fboundp s)))
                              (delete-dups my--calls))
                  #'string<)))))

(defun my-analyze-expression (&optional beg end)
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let ((b (my--expression-bounds)))
       (unless b (user-error "No enclosing expression at point"))
       (list (car b) (cdr b)))))
  (pcase-let* ((text (buffer-substring-no-properties beg end))
               (`(,g ,c ,p) (my-analyze-string text)))
    (with-output-to-temp-buffer "*sexp analysis*"
      (princ (format "Global variables (%d):\n  %s\n\nCustom variables (%d):\n  %s\n\nProcedure calls (%d):\n  %s\n"
                     (length g) (if g (mapconcat #'symbol-name g " ") "(none)")
                     (length c) (if c (mapconcat #'symbol-name c " ") "(none)")
                     (length p) (if p (mapconcat #'symbol-name p " ") "(none)"))))))
