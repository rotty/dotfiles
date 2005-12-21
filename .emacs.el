;; -*- Mode: Emacs-Lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			Basic Customization			    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put 'narrow-to-region 'disabled nil)

;; Not bound anymore by default?
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)

;;
;; http://opal.cabochon.com/~stevey/blog-rants/effective-emacs.html

(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(global-set-key [f5]  'call-last-kbd-macro)
(global-set-key [f11] 'goto-line)
(global-set-key [f12] 'toggle-transient-mark-mode)

(setq iswitchb-mode t)

(require 'tramp)
(eval-after-load "ange-ftp"
  '(tramp-disable-ange-ftp))


;; Support for running the Emacs in /usr/local/
;;
(eval-when-compile (require 'cl))	;case, push

(when (and (file-exists-p "/usr/share/emacs/site-lisp/debian-startup.el")
           (not (boundp 'debian-emacs-flavor))
           (= emacs-major-version 21)
	   (not (featurep 'xemacs)))
  ;; Pretend we're installed in the normal place.
  (pushnew "/usr/share/emacs/site-lisp" load-path :test #'equal)
  (pushnew "/usr/local/share/emacs/site-lisp" load-path :test #'equal)

  ;; Call the normal startup sequence.
  (defconst debian-emacs-flavor 'emacs21)
  (load "/usr/share/emacs/site-lisp/debian-startup.el")
  (debian-startup debian-emacs-flavor))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		Automatic Customization 			    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(add-log-mailing-address nil t)
 '(bbdb-north-american-phone-numbers-p nil)
 '(browse-url-browser-function (quote w3m-browse-url))
 '(browse-url-galeon-arguments (quote ("-n")))
 '(bsh-jar "/usr/share/java/bsh.jar")
 '(canlock-password "165523ce79fcd63dfb5b422d9a24488b238bef98")
 '(confirm-kill-emacs (quote y-or-n-p))
 '(debian-bug-always-CC-myself nil)
 '(debian-bug-helper-program (quote reportbug))
 '(debian-changelog-mailing-address "rotty@debian.org")
 '(dictionary-server "dict")
 '(ecb-layout-name "rotty-01")
 '(ecb-layout-nr 7)
 '(enable-multibyte-characters t)
 '(enable-recursive-minibuffers t)
 '(european-calendar-style nil)
 '(exec-path (quote ("/home/andy/bin" "/home/andy/.system/bin" "/usr/local/bin" "/usr/bin" "/bin" "/usr/bin/X11" "/usr/games" "/usr/lib/emacs/21.3/i386-linux")))
 '(general-holidays nil)
 '(glasses-face (quote bold))
 '(glasses-separator "")
 '(global-font-lock-mode t nil (font-lock))
 '(gnus-summary-mark-below -100)
 '(gnuserv-frame nil)
 '(ispell-local-dictionary "german-new8")
 '(jde-compiler (quote ("javac" "")))
 '(log-edit-confirm t)
 '(mail-source-delete-incoming t)
 '(mail-source-directory "~/mail/")
 '(mail-sources (quote ((directory :path "~/var/mail/incoming/" :suffix ".spool"))))
 '(max-lisp-eval-depth 20000)
 '(max-specpdl-size 100040)
 '(message-log-max 1000)
 '(message-user-fqdn "ivanova.rotty.yi.org")
 '(message-wide-reply-to-function (quote my-message-wide-reply-to-function))
 '(paren-mode (quote paren) nil (paren))
 '(quack-global-menu-p nil)
 '(quack-pltish-keywords-to-fontify (quote ("and" "begin" "call-with-current-continuation" "call-with-input-file" "call-with-output-file" "call/cc" "case" "case-lambda" "compound-unit/sig" "cond" "condition" "cond-expand" "define" "define*" "define-condition-type" "define-macro" "define-module" "define-public" "define-signature" "define-syntax" "define-syntax-set" "define-values" "define-values/invoke-unit/sig" "define-method" "define-generic" "define-class" "delay" "do" "else" "exit-handler" "guard" "if" "import" "lambda" "let" "let*" "let*-values" "let+" "let-keywords" "let-optional" "let-syntax" "let-values" "let/ec" "letrec" "letrec-values" "letrec-syntax" "match-lambda" "match-lambda*" "match-let" "match-let*" "match-letrec" "match-define" "mixin" "opt-lambda" "or" "override" "override*" "namespace-variable-bind/invoke-unit/sig" "parameterize" "private" "private*" "protect" "provide" "provide-signature-elements" "provide/contract" "public" "public*" "quote" "receive" "rename" "require" "require-for-syntax" "send" "send*" "setter" "set!" "set!-values" "signature->symbols" "super-instantiate" "syntax-case" "syntax-case*" "syntax-error" "syntax-rules" "unit/sig" "unless" "when" "with-handlers" "with-method" "with-syntax")))
 '(quack-programs (quote ("~/src/contrib/s42/light-scheme42" "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "ischeme48" "kali" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -M errortrace" "rs" "schem348" "schem48" "scheme" "scheme48" "scheme48 -cl ~/src/contrib/scheme48/src/scheme48/scheme/ffi/packages.scm -cl ~/src/contrib/g-wrap/=build/scheme48/g-wrap/gw/packages.scm -o g-wrap.gw.standard" "scheme48 -cl ~/src/contrib/scheme48/src/scheme48/scheme/ffi/packages.scm -o ffi" "scheme48 -cl ~/src/foreign/sunterlib-0.7/s48/exceptions/packages.scm -cl ~/src/contrib/tiny-clos/packages.scm -o tiny-goops -o tiny-test" "scheme48 -cl ~/src/foreign/sunterlib-0.7/s48/exceptions/packages.scm -cl ~/src/contrib/tiny-clos/src/packages.scm -o tiny-goops" "scheme48 -cl ~/src/foreign/sunterlib-0.7/s48/exceptions/packages.scm -cl ~/src/contrib/tiny-clos/src/packages.scm -o tiny-goops -o tiny-test" "scheme48 -h 10000000" "scheme48 -h 100000000" "scheme48vm" "scsh" "sisc" "stklos" "sxi" "~/src/contrib/s42/heavy-scheme42" "~/src/contrib/s42/run" "~/src/contrib/s42/run-scheme42" "~/src/contrib/scheme48/svn-gw/netapi/go")))
 '(quack-remap-find-file-bindings-p nil)
 '(sh-basic-offset 2)
 '(show-paren-delay 0)
 '(show-paren-mode (quote paren) nil (paren))
 '(tool-bar-mode nil nil (tool-bar))
 '(tramp-default-method "fcp")
 '(truncate-partial-width-windows nil)
 '(user-mail-address "a.rottmann@gmx.at"))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(paren-face-match ((((class color)) (:background "green"))))
 '(paren-face-mismatch ((((class color)) (:foreground "white" :background "red"))))
 '(paren-match ((t (:background "green"))))
 '(paren-mismatch ((t (:background "red"))))
 '(show-paren-match-face ((((class color)) (:background "green"))))
 '(show-paren-mismatch-face ((((class color)) (:background "red"))))
 '(variable-pitch ((t (:family "helvetica")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		Customization of Specific Packages		    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dired)

;;; ********************
;;; Load the auto-save.el package, which lets you put all of your autosave
;;; files in one place, instead of scattering them around the file system.
;;;
;(setq auto-save-directory (expand-file-name "~/autosave/")
;      auto-save-directory-fallback auto-save-directory
;      auto-save-hash-p nil
;      efs-auto-save t
;      efs-auto-save-remotely nil
;      ;; now that we have auto-save-timeout, let's crank this up
;      ;; for better interactive response.
;      auto-save-interval 2000
;      )
;; We load this afterwards because it checks to make sure the
;; auto-save-directory exists (creating it if not) when it's loaded.
;(require 'auto-save)

;; Extend load-path
(let ((basedir (concat (getenv "HOME") "/elisp")))
  (setq load-path 
	(append (list basedir
		      (concat basedir "/slime"))
		load-path)))

;; This adds additional extensions
(setq auto-mode-alist
      (append '(("\\.C$"  	. c++-mode)
		("\\.cc$" 	. c++-mode)
		("\\.hh$" 	. c++-mode)
		("\\.c$"  	. c-mode)
		("\\.h$"  	. c-mode)
		("\\.ptl$"	. python-mode)
		("\\.fs$"	. forth-mode)
		("\\.wml$" 	. html-mode)
		("\\.dtml$" 	. html-mode)
		("\\.stml$" 	. html-mode))
	      auto-mode-alist))


;;; Eiffel
(add-to-list 'auto-mode-alist '("\\.e\\'" . eiffel-mode))
(autoload 'eiffel-mode "eiffel" "Major mode for Eiffel programs" t)

;;; ********************
;;; AUC TeX

;; Resolve AUC Tex/outline-minor-mode prefix conflict
;;(setq outline-minor-mode-prefix "\C-o")
;;(setq outline-command-prefix "\C-o")
;;(global-set-key "\M-o" 'open-line)

;;(load "outline.el") ;; allout.el seems not to work with (at least) auctex

(add-hook 'LaTeX-mode-hook 'my-latex-mode-hook)

(defun my-latex-mode-hook ()
  (LaTeX-preview-setup)
  ;;(outline-minor-mode 1)
  (flyspell-mode 1))

;;; ********************
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

(add-hook 'scheme-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

;; paredit.el
(autoload 'enable-paredit-mode "paredit"
  "Turns on pseudo-structural editing of Lisp code."
  t)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook 'enable-paredit-mode)

;; test-suite/lib.scm test-case indenting
(put 'with-test-prefix 'scheme-indent-function 1)
(put 'with-interaction-environment 'scheme-indent-function 1)
(put 'with-benchmark-prefix 'scheme-indent-function 1)
(put 'benchmark 'scheme-indent-function 1)
(put 'letrec 'scheme-indent-function 1)
(put 'make-method 'scheme-indent-function 1)
(put 'add-method! 'scheme-indent-function 'defun)
(put 'let-slots 'scheme-indent-function 2)
(put 'let-optionals 'scheme-indent-function 2)
(put 'let-optionals* 'scheme-indent-function 2)
(put 'eval-when 'scheme-indent-function 1)
(put 'with-mutex 'scheme-indent-function 1)
(put 'with-fluids 'scheme-indent-function 1)
(put 'let-fluids 'scheme-indent-function 4)
(put 'with-handler 'scheme-indent-function 1)
(put 'destructure 'scheme-indent-function 1)
(put 'scmxlate-macro 'scheme-indent-function 1)
(put 'make-ctype 'scheme-indent-function 1)
(put 'match 'scheme-indent-function 1)
(put 'call-with-output-file/cleanup 'scheme-indent-function 1)
(put 'let-keywords 'scheme-indent-function 3)
(put 'dir-excursion 'scheme-indent-function 1)
(put 'tla-dir-excursion 'scheme-indent-function 2)
(put 'remote-apply 'scheme-indent-function 1)
(put 'remote-run! 'scheme-indent-function 1)
(put 'with-lock 'scheme-indent-function 1)
(put 'with-module 'scheme-indent-function 1)
(put 'with-current-directory 'scheme-indent-function 1)
(put 'call-with-file-and-dir 'scheme-indent-function 1)
(put 'call-with-file-dumpster 'scheme-indent-function 2)
(put 'call-with-file-retriever 'scheme-indent-function 2)
(put 'call-with-port-dumpster 'scheme-indent-function 2)
(put 'call-with-input-url 'scheme-indent-function 1)
(put 'restart-command-processor 'scheme-indent-function 2)
(put 'let-fluid 'scheme-indent-function 2)
(put 'with-condition-context 'scheme-indent-function 1)
(put 'with-exception-handler 'scheme-indent-function 1)
(put 'call-with-append-file 'scheme-indent-function 1)
(put 'call-with-process-output 'scheme-indent-function 1)
(put 'with-new-proposal 'scheme-indent-function 1)
(put 'with-cwd 'scheme-indent-function 1)
(put 'define-peephole-optimizer 'scheme-indent-function 2)
(put 'testeez 'scheme-indent-function 1)
(put 'test-true 'scheme-indent-function 1)
(put 'test/equal 'scheme-indent-function 1)
(put 'guard 'scheme-indent-function 1)

;;; ********************
;;; cc-mode (the mode you're in when editing C, C++, and Objective C files)

;; Tell cc-mode not to check for old-style (K&R) function declarations.
;; This speeds up indenting a lot.
(setq c-recognize-knr-p nil)

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(defconst visotech-c++-style
  '((c-basic-offset             . 4)
    (c-tab-always-indent        . t)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist     . ((substatement-open after)
                                      (brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    (c-offsets-alist            . ((arglist-close     . c-lineup-arglist)
                                   (substatement-open . 0)
                                   (case-label        . +)
                                   (access-label      . -)
                                   (block-open        . 0)
                                   (inclass           . ++)
                                   (innamespace       . 0)
                                   (inextern-lang     . 0)
                                   (label             . --)
                                   (inline-open . 0)
                                   (arglist-intro     . +)))
    (c-echo-syntactic-information-p . t)
    )
  "Visotech C++ Programming Style")

(c-add-style "visotech" visotech-c++-style)

;; My identing style
(defconst my-c++-style
  '((c-basic-offset		. 2)
    (c-tab-always-indent	. t)
    (c-comment-only-line-offset	. 0)
    (c-hanging-braces-alist	. ((substatement-open after)
                                      (brace-list-open)))
    (c-hanging-colons-alist	. ((member-init-intro before)
				   (inher-intro)
				   (case-label after)
				   (label after)
				   (access-label after)))
    (c-cleanup-list		. (scope-operator
				   empty-defun-braces
				   defun-close-semi))
    (c-offsets-alist		. ((arglist-close     . c-lineup-arglist)
				   (substatement-open . 0)
				   (case-label        . +)
				   (access-label      . -)
				   (block-open        . 0)
				   (inclass           . ++)
                                   (innamespace       . 0)
				   (inextern-lang     . 0)
				   (label             . --)
				   (inline-open . 0)
                                   (arglist-intro     . +8)))
    (c-echo-syntactic-information-p . t)
    )
  "My C/C++ Programming Style")


(c-add-style "personal" my-c++-style)

(defun my-c++-mode-hook ()
  ;; Add personal style and set it for the current buffer
  ;; FIXME!
  t)

(defun my-c-mode-common-hook ()
  (hs-minor-mode 1)
  (setq case-fold-search nil)
  ;; Offset customizations not in my-c-style
  (c-set-offset 'member-init-intro '++)
  ;; other customizations
  (setq tab-width 8
        ;; this will make sure spaces are used instead of tabs
        indent-tabs-mode nil)
  ;; we like auto-newline and hungry-delete
  ;;(c-toggle-auto-hungry-state 1)
  ;; keybindings for C, C++, and Objective-C.  We can put these in
  ;; c-mode-map because c++-mode-map and objc-mode-map inherit it
  (define-key c-mode-map "\C-m" 'newline-and-indent)
  ;; somewhat better suggestion for compile-command
  ;;(make-local-variable 'compile-command)
  (setq compile-command
        (concat "make -C "
                (substring (home-abbrev-file-name
                            (file-name-directory buffer-file-name)) 0 -1)))
  )

(require 'c-decl)

;; JDE
;;;
;; (require 'jde)
;; (defun my-java-mode-hook ()
;;   (c-set-offset 'inclass '+))

;; (add-hook 'java-mode-hook 'my-java-mode-hook)
;;(require 'jde-ant)

;; C#
;;
(autoload 'csharp-mode "csharp-mode" 
  "Major mode for editing C# code." t)
(setq auto-mode-alist (cons '( "\\.cs\\'" . csharp-mode ) auto-mode-alist))

(defun my-csharp-mode-hook ()
  ;; Add personal style and set it for the current buffer
  (c-add-style "PERSONAL" my-c++-style t)
  (c-set-offset 'inclass '+))

(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)


;; CSS-Mode
;;
(setq cssm-indent-function #'cssm-c-style-indenter)

;; PSGML
(setq sgml-set-face t)
(setq sgml-indent-step 1)

;; Tuareg-Mode (OCAML)
(autoload 'tuareg-mode "tuareg" nil t)


;;; ********************
;;; OO-Browser
;;;
;(load "br-start")

;;; ********************
;;; Load crypt, which is a package for automatically decoding and reencoding
;;; files by various methods - for example, you can visit a .Z or .gz file,
;;; edit it, and have it automatically re-compressed when you save it again.
;;; 
(setq crypt-encryption-type 'gpg   ; default encryption mechanism
      crypt-confirm-password t	   ; make sure new passwords are correct
      ;crypt-never-ever-decrypt t  ; if you don't encrypt anything, set this to
				   ; tell it not to assume that "binary" files
				   ; are encrypted and require a password.
      )
(require 'crypt)

;; pcomplete support for tla
;;(require 'pcmpl-tla)

;;; ****************
;;: Dictionary
;;;;
(global-set-key [f3] 'dictionary-lookup-definition)

;;; ******************
;;; Mail
;;;
(add-hook 'mail-setup-hook 'mail-abbrevs-setup)

;;; ********************
;;; Edebug is a source-level debugger for emacs-lisp programs.
;;;
(define-key emacs-lisp-mode-map "\C-xx" 'edebug-defun)

;;; ********************
;;; lazy-lock is a package which speeds up the highlighting of files
;;; by doing it "on-the-fly" -- only the visible portion of the
;;; buffer is fontified.  The results may not always be quite as
;;; accurate as using full font-lock or fast-lock, but it's *much*
;;; faster.  No more annoying pauses when you load files.

;(add-hook 'font-lock-mode-hook 'turn-on-lazy-lock)
;; I personally don't like "stealth mode" (where lazy-lock starts
;; fontifying in the background if you're idle for 30 seconds)
;; because it takes too long to wake up again on my piddly Sparc 1+.
;(setq lazy-lock-stealth-time nil)


;(toggle-global-lazy-font-lock-mode) ;; set it to true

(require 'bbdb)
(bbdb-initialize 'gnus 'message 'w3)
(require 'bbdb-remind)

;; Some goodies
;;(require 'follow-mouse)


;;
;; MMM-Mode
;;(require 'mmm-auto)
;;(setq mmm-global-mode 'maybe)

;; (setq my-mason-project-dirs '("/home/andy/uni/vienna/WebServiceEngeneering/src/"))


;; (mapc (lambda (dir)
;; 	(add-to-list 'auto-mode-alist (cons dir 'sgml-mode))
;; 	(mmm-add-mode-ext-class 'sgml-mode dir 'mason))
;;       my-mason-project-dirs)

;;
;; Emacs Code Browser & Semantic
;;
;;(add-hook 'semantic-init-hooks 'senator-minor-mode)


;; Guile Scheme
;;(require 'guile-scheme)

;; show paren mode
(show-paren-mode t)

;; Emacs speaks statistics
;(require 'ess-site)

;; buffer stack (overwriting bindings for scroll-left and scroll-right)
(require 'buffer-stack)
(global-set-key [C-next] `buffer-stack-down)
(global-set-key [C-prior] `buffer-stack-up)

;; GNU Arch
;;(require 'xtla)

;;
;; emacs-goodies-el
;;

;; dirvars
(require 'dirvars)

;; projects (nicer buffer names)
(require 'projects)

;;
;; Doxymacs
(require 'doxymacs)
(add-hook 'c-mode-common-hook 'doxymacs-mode)
(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
(setq doxymacs-use-external-xml-parser t)

;; Diary
(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'list-diary-entries-hook 'sort-diary-entries)
(add-hook 'list-diary-entries-hook 'include-other-diary-files)
(add-hook 'mark-diary-entries-hook 'mark-included-diary-files)

;; Emacs-Wiki
(require 'table)
(load "emacs-wiki-config")

;; Planner
;;(load "planner-config")

;; Supercite
;(autoload 'sc-cite-original     "supercite" "Supercite 3.1" t)
;(autoload 'sc-submit-bug-report "supercite" "Supercite 3.1" t)

;(add-hook 'mail-citation-hook 'sc-cite-original)

;; ITLA
(autoload 'itla "itla" "Run itla." t)

;;
;; gpg.el from Oort Gnus
;;
(require 'gpg)
;; Your passphrase timeout. Set it to zero if Emacs
;; shouldn't store your passphrase
(setq gpg-passphrase-timeout 120)
(setq gpg-temp-directory "~/.gnupg/tmp/")
;; Your default key for signing emails.
(setq gpg-default-key-id "01FD5B62")

(defun irc ()
  (interactive)
  (erc-select :server "irc.freenode.net" :nick "rotty")
  (erc-select :server "irc.oftc.net" :nick "rotty")
  (erc-select :server "irc.gimp.org" :nick "rotty"))



(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Use UTF-8 for file name encoding
(setq file-name-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Misc helpful functions
(load "util-funcs")

(setq write-file-hooks (cons 'update-copyright-with-queries write-file-hooks))

;; Load saved Emacs session
;;(desktop-load-default)
;;(desktop-read)
