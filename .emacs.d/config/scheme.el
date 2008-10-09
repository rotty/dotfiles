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
  
  (local-set-key (kbd "M-/") (make-hippie-expand-function
			      '(try-my-dabbrev-substring
				try-expand-dabbrev-visible
				try-expand-dabbrev-from-kill
				try-expand-dabbrev-all-buffers
				try-complete-file-name-partially
				try-complete-file-name))))

(dolist (hook '(emacs-lisp-mode-hook lisp-mode-hook scheme-mode-hook))
  (add-hook hook 'my-lispy-mode-hook))

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
	   (and-let* 1)))
  (put (car hint) 'scheme-indent-function (cadr hint)))
