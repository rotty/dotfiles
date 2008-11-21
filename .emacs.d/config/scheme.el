;;(eval-after-load "scheme" '(modify-syntax-entry ?, "'   " scheme-mode-syntax-table))

;;; Quack-el (extended Scheme support, http://www.neilvandyke.org/quack/)
(require 'quack)

;; bracketphobia, http://www.emacswiki.org/cgi-bin/wiki/bracketphobia.el
(require 'bracketphobia)

(autoload 'run-scheme "cmuscheme48" "Run an inferior Scheme48 process." t)
(setq scheme-program-name "scheme48")
(dolist (elt '(("\\.sls$" . scheme-mode)
	       ("\\.sps$" . scheme-mode)))
  (add-to-list 'auto-mode-alist elt))

;; paredit.el
(require 'paredit)

;; override these paredit bindings, they annoy the hell out of me
;; (they are bound to other key sequences as well, so no
;; functionality loss)
(define-key paredit-mode-map (kbd "C-<left>") nil)
(define-key paredit-mode-map (kbd "C-<right>") nil)

(defun my-lispy-mode-hook ()
  (enable-paredit-mode)
  (set (make-local-variable 'comment-add) 1)
  (local-set-key (kbd "M-/") (make-hippie-expand-function
			      '(try-my-dabbrev-substring
				try-expand-dabbrev-visible
				try-expand-dabbrev-from-kill
				try-expand-dabbrev-all-buffers
				try-complete-file-name-partially
				try-complete-file-name))))

(dolist (hook '(emacs-lisp-mode-hook lisp-mode-hook scheme-mode-hook))
  (add-hook hook 'my-lispy-mode-hook))


;; ikarus-script
(add-hook 'scheme-mode-hook 'ikarus-script-setup-buffer)

(eval-after-load 'scheme
  '(progn
     (define-key scheme-mode-map (kbd "C-c i") 'ikarus-run-script)
     (define-key scheme-mode-map (kbd "C-c r") 'ikarus-rerun-script)))


;; scheme-complete
(autoload 'scheme-smart-complete "scheme-complete" nil t)
(autoload 'scheme-complete-or-indent "scheme-complete" nil t)
(autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)

(eval-after-load 'scheme
  '(progn (define-key scheme-mode-map "\t" 'scheme-complete-or-indent)))

(add-hook 'scheme-mode-hook
   (lambda ()
     (make-local-variable 'eldoc-documentation-function)
     (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
     (eldoc-mode)))



(put 'scheme48-package 'safe-local-variable 'symbolp)

(setq quack-pltish-keywords-to-fontify
      '("and" "begin" "call-with-current-continuation" "call-with-input-file" "call-with-output-file" "call/cc" "case" "case-lambda" "compound-unit/sig" "cond" "condition" "cond-expand" "define" "define/optional" "define-condition-type" "define-macro" "define-module" "define-public" "define-signature" "define-syntax" "define-syntax-set" "define-values" "define-values/invoke-unit/sig" "define-method" "define-generic" "define-class" "delay" "do" "else" "exit-handler" "guard" "if" "import" "lambda" "let" "let*" "let*-values" "let+" "let-keywords" "let-optional" "let-syntax" "let-values" "let/ec" "letrec" "letrec-values" "letrec-syntax" "library" "match-lambda" "match-lambda*" "match-let" "match-let*" "match-letrec" "match-define" "mixin" "opt-lambda" "or" "override" "override*" "namespace-variable-bind/invoke-unit/sig" "parameterize" "private" "private*" "protect" "provide" "provide-signature-elements" "provide/contract" "public" "public*" "quote" "receive" "rename" "require" "require-for-syntax" "send" "send*" "setter" "set!" "set!-values" "signature->symbols" "super-instantiate" "syntax-case" "syntax-case*" "syntax-error" "syntax-rules" "unit/sig" "unless" "when" "with-handlers" "with-method" "with-syntax"))



(dolist (hint
	 '((with-test-prefix 1)
	   (with-interaction-environment 1)
	   (with-benchmark-prefix  1)
	   (letrec 1)
	   (make-method 1)
	   (add-method! defun)
	   (let-slots 2)
	   (let-optionals 2)
	   (let-optionals* 2)
	   (eval-when 1)
	   (with-mutex 1)
	   (with-fluids 1)
	   (let-fluids 4)
	   (with-handler 1)
	   (destructure 1)
	   (scmxlate-macro 1)
	   (make-ctype 1)
	   (match 1)
	   (call-with-output-file/cleanup 1)
	   (let-keywords 3)
	   (dir-excursion 1)
	   (tla-dir-excursion 2)
	   (remote-apply 1)
	   (remote-run! 1)
	   (with/fc 1)
	   (with-lock 1)
	   (with-module 1)
	   (with-working-directory 1)
	   (open-tcp-listener-accept-loop 1)
	   (call-with-file-and-dir 1)
	   (call-with-file-dumpster 2)
	   (call-with-file-retriever 2)
	   (call-with-port-dumpster 2)
	   (call-with-input-url 1)
	   (call-with-file-fasdump-port 2)
	   (call-with-file-fasload-port 2)
	   (call-with-string-output-port)
	   (call-with-input-string 1)
	   (call-with-port 1)
	   (restart-command-processor 2)
	   (let-fluid 2)
	   (with-condition-context 1)
	   (with-exception-handler 1)
	   (call-with-append-file 1)
	   (call-with-current-noise-port 1)
	   (call-with-process-output 2)
	   (call-with-process-input 2)
	   (with-new-proposal 1)
	   (with-cwd 1)
	   (define-peephole-optimizer 2)
	   (testeez 1)
	   (test-true 1)
	   (test-eval 1)
	   (test-define 2)
	   (test/equal 1)
	   (test/equiv 1)
	   (parse 1)
	   (object 1)
	   (set-standard-read-macro! 2)
	   (add-tests-with-string-output 1)
	   (guard 1)
	   (library 1)
	   (trace-define 1)
	   (trace-lambda 2)
	   (let-callouts 2)
	   (and-let* 1)
	   (send 1)
	   (flags-case 1)
	   (let-attributes 3)
	   (let-accessors 2)
	   (let-ginstance-fields 2)))
  (put (car hint) 'scheme-indent-function (cadr hint)))

(require 'ikarus-script)

(eval-after-load "scheme" '(progn

;;; This is a *slightly* modified version of what is in scheme.el,
;;; which is itself a slight modification of `lisp-indent-function'
;;; from lisp-mode.el.  Gee, you'd think that someone would think of
;;; the notion of 'abstraction' here...

(defun scheme-indent-function (indent-point state)
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (downcase         ;** downcasage added by TRC
                       (buffer-substring (point)
                                         (progn (forward-sexp 1) (point)))))
            method)
        (setq method (or (get (intern-soft function) 'scheme-indent-function)
                         (get (intern-soft function) 'scheme-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ;** WITH-... & CALL-WITH-... forms added by TRC
              ((or (eq method 'with-...)
                   (eq method 'call-with-...)
                   (and (null method)
                        (or (and (> (length function) 5)
                                 (string-match "\\`with-" function))
                            (and (> (length function) 9)
                                 (string-match "\\`call-with-" function)))))
               (lisp-indent-withform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method state indent-point normal-indent)))))))

;;; This could be generalized to negative special form indent methods; e.g.,
;;;
;;;   (put 'with-frobbotzim 'scheme-indent-function -2)
;;;
;;; and then
;;;
;;;   (with-frobbotzim frob grovel
;;;       full lexical
;;;       mumble chumble
;;;       spuzz
;;;     (lambda (foo) ...)
;;;     (lambda (bar) ...))
;;;
;;; That is, the last two subforms would be indented two spaces, whereas all
;;; preceding subforms would get four spaces.

(defun lisp-indent-withform (state indent-point)
  (if (not (and (boundp 'paredit-mode)
                paredit-mode))
      ;; If we're not in paredit mode, it's not really safe to go backwards
      ;; from the end and to try to indent based on that, since there may not
      ;; be an end to work backwards from (i.e. the structure not be valid).
      (lisp-indent-defform state indent-point)
    (goto-char (nth 1 state))
    (let ((body-column (+ (current-column)
                          lisp-body-indent)))
      (forward-sexp 1)
      (backward-char 1)
      (backward-sexp 1)
      (skip-chars-backward " \t" (point-at-bol))
      (if (= (point) indent-point)
          body-column
          ;; If it's not the last argument, then we must specify not only the
          ;; column to indent to but also the start of the containing sexp,
          ;; which implies (don't ask me how) that any *following* subforms
          ;; must be indented separately, and not just on this column.  This
          ;; allows C-M-q to know to indent the penultimate arguments with four
          ;; spaces, but to keep recomputing the indentation so that it doesn't
          ;; assume the last one will go to the same column, which is a wrong
          ;; assumption.
          (list (+ body-column lisp-body-indent)
                (nth 1 state))))))

(put 'let-fluids 'scheme-indent-function 'with-...)

;;; Nested foof-loop forms

(put 'iterate 'scheme-indent-function 'with-...)
(put 'iterate! 'scheme-indent-function 'with-...)
(put 'iterate* 'scheme-indent-function 'with-...)
(put 'iterate-values 'scheme-indent-function 'with-...)
(put 'lazy-recur 'scheme-indent-function 'with-...)
(put 'lazy-recur* 'scheme-indent-function 'with-...)
(put 'recur 'scheme-indent-function 'with-...)
(put 'recur* 'scheme-indent-function 'with-...)

;;; This is silly, but so would altering the definition of
;;; `scheme-indent-function' yet again to include a test for
;;; `collect-...'.  Better would be to have a table mapping regular
;;; expressions to indent functions, as Edwin has.  But this is
;;; expedient for now.

(put 'collect-average 'scheme-indent-function 'with-...)
(put 'collect-display 'scheme-indent-function 'with-...)
(put 'collect-list 'scheme-indent-function 'with-...)
(put 'collect-list! 'scheme-indent-function 'with-...)
(put 'collect-list-into! 'scheme-indent-function 'with-...)
(put 'collect-list-reverse 'scheme-indent-function 'with-...)
(put 'collect-max 'scheme-indent-function 'with-...)
(put 'collect-min 'scheme-indent-function 'with-...)
(put 'collect-product 'scheme-indent-function 'with-...)
(put 'collect-stream 'scheme-indent-function 'with-...)
(put 'collect-string 'scheme-indent-function 'with-...)
(put 'collect-string-of-length 'scheme-indent-function 'with-...)
(put 'collect-sum 'scheme-indent-function 'with-...)
(put 'collect-vector 'scheme-indent-function 'with-...)
(put 'collect-vector-of-length 'scheme-indent-function 'with-...)

;;; This one doesn't follow the same pattern as the others, because
;;; there is no expression; (COLLECT-COUNT ...) is the same as
;;; (COLLECT-SUM ... 1).

(put 'collect-count 'scheme-indent-function 0)

;;;; RECEIVE Indentation

;;; (RECEIVE <bvl>           ; Line up BVL & producer.
;;;          <producer>
;;;   <body>)

(defun scheme-indent-receive (state indent-point normal-indent)
  (let ((containing-form-start (nth 1 state))
        (i 0)
        containing-form-column)
    ;; <snip documentation>
    (goto-char containing-form-start)
    (setq containing-form-column (current-column))
    (forward-char 1)
    (forward-sexp 1)
    ;; Now find the start of the last form.
    (parse-partial-sexp (point) indent-point 1 t)
    (while (and (< (point) indent-point)
                (condition-case ()
                    (progn
                      (setq i (1+ i))
                      (forward-sexp 1)
                      (parse-partial-sexp (point) indent-point 1 t))
                  (error nil))))
    ;; Point is sitting on first character of last (or count) sexp.
    (cond ((= i 0)
           (+ containing-form-column (* 2 lisp-body-indent)))
          ((= i 1) (list normal-indent containing-form-start))
          (t (+ containing-form-column lisp-body-indent)))))

(put 'receive 'scheme-indent-function 'scheme-indent-receive)

))        ; end of eval-after-load for scheme.el
