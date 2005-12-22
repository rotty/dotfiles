;;; Quack-el (extended Scheme support)
(require 'quack)

(autoload 'run-scheme "cmuscheme48" "Run an inferior Scheme48 process." t)

;; Slime
(eval-after-load "slime"
  '(progn
     (setq scheme48-program-name "~/src/contrib/s42/light-scheme42")
     (load "~/src/foreign/slime48/slime48")))

(autoload 'slime-mode "slime" "Activate slime-mode" t)
(autoload 'slime "slime" "Activate slime-mode" t)

;; (add-hook 'scheme-mode-hook (lambda () (slime-mode t)))
;; (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

;; paredit.el
(autoload 'enable-paredit-mode "paredit"
  "Turns on pseudo-structural editing of Lisp code."
  t)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook 'enable-paredit-mode)

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
	   (with-lock 1)
	   (with-module 1)
	   (with-current-directory 1)
	   (call-with-file-and-dir 1)
	   (call-with-file-dumpster 2)
	   (call-with-file-retriever 2)
	   (call-with-port-dumpster 2)
	   (call-with-input-url 1)
	   (restart-command-processor 2)
	   (let-fluid 2)
	   (with-condition-context 1)
	   (with-exception-handler 1)
	   (call-with-append-file 1)
	   (call-with-process-output 1)
	   (with-new-proposal 1)
	   (with-cwd 1)
	   (define-peephole-optimizer 2)
	   (testeez 1)
	   (test-true 1)
	   (test/equal 1)
	   (guard 1)))
  (put (car hint) 'scheme-indent-function (cadr hint)))
