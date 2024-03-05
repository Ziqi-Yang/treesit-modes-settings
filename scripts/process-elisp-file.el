;; please run this file at the root of the project directory
;; example run command see /justfile `process' sub-command
(require 'cl-macs)

(defconst MELPA-REPOS-DIR "./vendor/melpa")
(defconst EMACS-DIR "./vendor/emacs-master/")

(defvar lang-settings nil)

(defun get-all-processing-files()
  (let ((melpa-repo-dirs (directory-files MELPA-REPOS-DIR t (rx-to-string '(not (any "." "..")))))
        process-files)
    (dolist (repo-dir melpa-repo-dirs)
      (setq process-files
            (append process-files
                    (list (file-name-concat repo-dir (concat (file-name-nondirectory repo-dir) ".el"))))))
    (dolist (file (directory-files EMACS-DIR t (rx-to-string '(not (any "." "..")))))
      (setq process-files (append process-files (list file))))
    process-files))

(defmacro moe-dolist (spec &rest body)
  "Moe dolist.
A moe `dolist' that can handle more asdfexpressions like
`(else if_statement . alternative)'.
SPEC BODY."
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  (unless (consp spec)
    (signal 'wrong-type-argument (list 'consp spec)))
  (unless (<= 2 (length spec) 3)
    (signal 'wrong-number-of-arguments (list '(2 . 3) (length spec))))
  (let ((tail (make-symbol "tail")))
    `(let ((,tail ,(nth 1 spec)))
       (while ,tail
         (if (consp ,tail)
             (let ((,(car spec) (car ,tail)))
               ,@body
               (setq ,tail (cdr ,tail)))
           (let ((,(car spec) ,tail))
             ,@body
             (setq ,tail nil))))
       ,@(cddr spec))))

(defun traverse-lisp (lang l-exp)
  (when (and (consp l-exp) (consp (cdr l-exp)))  ; make sure it's actually a list, not (a . b), so that `dolist' works
    (cond
     ((eq (car l-exp) 'provide)
      (add-lang-setting
       lang ':feature (cadr (cadr l-exp))))
     ((eq (car l-exp) 'setq-local)
      (let (key value)
        (cl-loop for index from 1 to (1- (length l-exp)) by 2
                 do
                 (setq key (nth index l-exp)
                       value (nth (1+ index) l-exp))
                 (when (memq key '(treesit-font-lock-settings
                                   treesit-simple-indent-rules
                                   treesit-font-lock-feature-list))
                   (add-lang-setting
                    lang
                    (intern (concat ":" (symbol-name key)))
                    value))))))
    (moe-dolist (elm l-exp)
      ;; (message "> %s %s" elm (type-of elm))  ; (debug) print each expression
      (when (consp elm)
        (traverse-lisp lang elm)))))

(defun process-file (file-name)
  (message "Processing file %s" file-name)
  (let ((lang (get-lang-symbol-from-filename file-name))
        file-lisp)
    (with-temp-buffer
      (insert-file-contents-literally file-name)
      (goto-char (point-min))
      (condition-case err
          (while-let ((exp (condition-case inner-err
                               (read (current-buffer))
                             ;; variables like `rust-ts-mode-prettify-symbols-alist' will cause this error
                             ;; it's because ?∧ will be translated into something like `?\330\220\223' before read
                             (invalid-read-syntax
                              (message "> %s" (error-message-string inner-err)))
                             (t (signal (car inner-err) (cdr inner-err))))))
            (setq file-lisp (append file-lisp (list exp))))
        (end-of-file nil)
        (t (signal (car err) (cdr err))))
      (traverse-lisp lang file-lisp))))

(defun process()
  ;; (process-file "./vendor/melpa/nushell-ts-mode/nushell-ts-mode.el")
  ;; (process-file "./vendor/emacs-master/c-ts-mode.el")
  
  (dolist (filename (get-all-processing-files))
    (process-file filename))
  (message "%s" lang-settings))

(defun get-lang-symbol-from-filename (filename)
  (intern (nth 0 (split-string (file-name-nondirectory filename) "-"))))

(defun add-lang-setting (lang key value)
  (unless (assoc lang lang-settings)
    (setq lang-settings
          (append lang-settings
                  `((,lang ,(list  ; every time create a list
                             :feature
                             nil
                             :treesit-font-lock-settings nil
                             :treesit-font-lock-feature-list nil
                             :treesit-simple-indent-rules nil))))))
  (plist-put
   (cadr (assoc lang lang-settings))
   key value))


