;; -*- Mode: Emacs-Lisp -*-

;;; This is a sample .emacs file.
;;;
;;; The .emacs file, which should reside in your home directory, allows you to
;;; customize the behavior of Emacs.  In general, changes to your .emacs file
;;; will not take effect until the next time you start up Emacs.  You can load
;;; it explicitly with `M-x load-file RET ~/.emacs RET'.
;;;
;;; There is a great deal of documentation on customization in the Emacs
;;; manual.  You can read this manual with the online Info browser: type
;;; `C-h i' or select "Emacs Info" from the "Help" menu.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			Basic Customization			    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable the command `narrow-to-region' ("C-x n n"), a useful
;; command, but possibly confusing to a new user, so it's disabled by
;; default.
(put 'narrow-to-region 'disabled nil)

;;; Define a variable to indicate whether we're running XEmacs/Lucid Emacs.
;;; (You do not have to defvar a global variable before using it --
;;; you can just call `setq' directly like we do for `emacs-major-version'
;;; below.  It's clearer this way, though.)

(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

;; set up the function keys to do common tasks to reduce Emacs pinky
;; and such.

;; Not bound anymore by default?
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)

;; Make F1 invoke help
(global-set-key [f1] 'help-command)
;; Make F2 be `undo'
(global-set-key [f2] 'undo)
;; Make F3 be `find-file'
;; Note: it does not currently work to say
;;   (global-set-key 'f3 "\C-x\C-f")
;; The reason is that macros can't do interactive things properly.
;; This is an extremely longstanding bug in Emacs.  Eventually,
;; it will be fixed. (Hopefully ..)
;;(global-set-key [f3] 'find-file)

;; Make F4 be "mark", F5 be "copy", F6 be "paste"
;; Note that you can set a key sequence either to a command or to another
;; key sequence.
(global-set-key [f4] 'set-mark-command)
(global-set-key [f5] "\M-w")
(global-set-key [f6] "\C-y")

;; Shift-F4 is "pop mark off of stack"
(global-set-key [(shift f4)] (lambda () (interactive) (set-mark-command t)))

;; Make F7 be `save-buffer'
(global-set-key [f7] 'save-buffer)

;; Make F8 be "start macro", F9 be "end macro", F10 be "execute macro"
(global-set-key [f8] 'start-kbd-macro)
(global-set-key [f9] 'end-kbd-macro)
(global-set-key [f10] 'hs-hide-block)
(global-set-key [(shift f10)] 'hs-show-block)

(global-set-key [f11] 'goto-line)
(global-set-key [f12] 'toggle-transient-mark-mode)

;; Here's an alternative binding if you don't use keyboard macros:
;; Make F8 be `save-buffer' followed by `delete-window'.
;;(global-set-key 'f8 "\C-x\C-s\C-x0")

;; If you prefer delete to actually delete forward then you want to
;; uncomment the next line (or use `Customize' to customize this).
;; (setq delete-key-deletes-forward t)


;;; Older versions of emacs did not have these variables
;;; (emacs-major-version and emacs-minor-version.)
;;; Let's define them if they're not around, since they make
;;; it much easier to conditionalize on the emacs version.

(if (and (not (boundp 'emacs-major-version))
	 (string-match "^[0-9]+" emacs-version))
    (setq emacs-major-version
	  (string-to-int (substring emacs-version
				    (match-beginning 0) (match-end 0)))))
(if (and (not (boundp 'emacs-minor-version))
	 (string-match "^[0-9]+\\.\\([0-9]+\\)" emacs-version))
    (setq emacs-minor-version
	  (string-to-int (substring emacs-version
				    (match-beginning 1) (match-end 1)))))

;;; Define a function to make it easier to check which version we're
;;; running.

(defun running-emacs-version-or-newer (major minor)
  (or (> emacs-major-version major)
      (and (= emacs-major-version major)
	   (>= emacs-minor-version minor))))

(cond ((and running-xemacs
	    (running-emacs-version-or-newer 19 6))
       ;;
       ;; Code requiring XEmacs/Lucid Emacs version 19.6 or newer goes here
       ;;
       ))

(cond ((>= emacs-major-version 19)
       ;;
       ;; Code for any vintage-19 emacs goes here
       ;;
       ))

(cond ((and (not running-xemacs)
	    (>= emacs-major-version 19))
       ;;
       ;; Code specific to FSF Emacs 19 (not XEmacs/Lucid Emacs) goes here
       ;;
       ))

(cond ((< emacs-major-version 19)
       ;;
       ;; Code specific to emacs 18 goes here
       ;;
       ))

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
 '(add-log-mailing-address nil)
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
 '(quack-programs (quote ("~/src/contrib/scheme48/svn-gw/netapi/go" "ischeme48" "scheme48 -h 100000000" "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kali" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -M errortrace" "rs" "scheme" "scheme48" "scheme48 -cl ~/src/contrib/scheme48/src/scheme48/scheme/ffi/packages.scm -cl ~/src/contrib/g-wrap/=build/scheme48/g-wrap/gw/packages.scm -o g-wrap.gw.standard" "scheme48 -cl ~/src/contrib/scheme48/src/scheme48/scheme/ffi/packages.scm -o ffi" "scheme48 -cl ~/src/foreign/sunterlib-0.7/s48/exceptions/packages.scm -cl ~/src/contrib/tiny-clos/packages.scm -o tiny-goops -o tiny-test" "scheme48 -cl ~/src/foreign/sunterlib-0.7/s48/exceptions/packages.scm -cl ~/src/contrib/tiny-clos/src/packages.scm -o tiny-goops" "scheme48 -cl ~/src/foreign/sunterlib-0.7/s48/exceptions/packages.scm -cl ~/src/contrib/tiny-clos/src/packages.scm -o tiny-goops -o tiny-test" "scheme48 -h 10000000" "scheme48vm" "scsh" "sisc" "stklos" "sxi")))
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
;;; Load the default-dir.el package which installs fancy handling
;;;  of the initial contents in the minibuffer when reading
;;; file names.

(if (and running-xemacs
	 (or (and (= emacs-major-version 20) (>= emacs-minor-version 1))
	     (and (= emacs-major-version 19) (>= emacs-minor-version 15))))
    (require 'default-dir))

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
		      (concat basedir "/guile"))
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
;;(require 'quack)

(autoload 'run-scheme "cmuscheme48" "Run an inferior Scheme48 process." t)

;; test-suite/lib.scm test-case indenting
(put 'with-test-prefix 'scheme-indent-function 1)
(put 'with-interaction-environment 'scheme-indent-function 1)
(put 'with-benchmark-prefix 'scheme-indent-function 1)
(put 'benchmark 'scheme-indent-function 1)
(put 'letrec 'scheme-indent-function 1)
(put 'make-method 'scheme-indent-function 1)
(put 'add-method! 'scheme-indent-function 'defun)
(put 'let-slots 'scheme-indent-function 2)
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
(put 'restart-command-processor 'scheme-indent-function 2)
(put 'let-fluid 'scheme-indent-function 2)
(put 'with-condition-context 'scheme-indent-function 1)
(put 'with-exception-handler 'scheme-indent-function 1)
(put 'call-with-append-file 'scheme-indent-function 1)

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

(c-add-style "Visotech" visotech-c++-style)

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


(c-add-style "PERSONAL" my-c++-style)

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
(defvar enable-dirvar-variables t) ; Workaround bug

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

;; Yacas
(autoload 'yacas "yacas" "Running Yacas" t)

;; ITLA
(autoload 'itla "itla" "Run itla." t)

;; paredit.el
(autoload 'enable-paredit-mode "paredit"
  "Turns on pseudo-structural editing of Lisp code."
  t)
(add-hook 'scheme-mode-hook 'enable-paredit-mode)

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

;; Tiny Tools
;(pushnew "~/elisp/tiny" load-path :test 'string=)
;(load "tinypath.el")

(defun irc ()
  (interactive)
  (erc-select :server "irc.freenode.net" :nick "rotty"))
  ;;(erc-select :server "irc.oftc.net" :nick "rotty")
  ;;(erc-select :server "irc.gimp.org" :nick "rotty"))



;;
;; w3m-el proxy setting
;;
;(eval-after-load "w3m"
;  '(setq w3m-command-arguments
;	 (nconc w3m-command-arguments
;		'("-o" "http_proxy=http://garibaldi.rhinosaur.lan:3128/"))))


(put 'upcase-region 'disabled nil)

(put 'downcase-region 'disabled nil)

;; Use UTF-8 for file name encoding
(setq file-name-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;; Some utilities

(defun devhelp-word-at-point ()
  "runs devhelp"
  (interactive)
  (start-process-shell-command "devhelp" nil "devhelp" "-s" (current-word))
  )

; Example: bind F7 to start devhelp and search for the word at the point.
(global-set-key [f7] 'devhelp-word-at-point)

(load "util-funcs")


;; Update copyright

(defconst current-year (substring (current-time-string) -4)
  "String representing the current year.")
(defconst last-year (int-to-string (- (string-to-int current-year) 1))
  "String representing the current year (presuming that the current year is not 1 AD, which hopefully will continue to be the case indefinitely).")
(defvar current-gpl-version "2"
  "String representing the current version of the GPL.")
(defvar copyright-regex "[Cc]opyright\\s *\\(([Cc])\\)?\\(\\s *[0-9]+\\s *\\(-\\s *[0-9]+\\s *\\)?,\\s *\\)*\\s *\\(\\([0-9]+\\)\\s *-\\)?\\s *\\([0-9]+\\)"
  "Regular expression to match common copyright declarations, extracting the final year(s).")
;; Note: paren expr. #5 is the first year of the last dashed pair, if
;; any; paren expr. #6 is the last year.

(defun update-copyright-with-queries ()
  "My version of update-copyright."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (and (re-search-forward "[i]s free software"
                              nil t)
           (not (eq major-mode 'rmail-mode))
           (let ((limit (point)))
             (goto-char (point-min))
             (re-search-forward copyright-regex
              limit t))
    (let ((final-year (match-string 6))
   (final-range-start (match-string 5)))
      (when (and (not (string= final-year current-year))
   (progn (goto-char (point-min))
          (sit-for 0)
          (y-or-n-p (format "Update copyright (last %s)? " final-year))))
        (if (string= final-year last-year)
     (if final-range-start
         (progn
    (goto-char (match-end 6))
    (delete-region (match-beginning 6) (match-end 6))
    (insert current-year))
       (progn
         (goto-char (match-end 6))
         (insert "-")
         (insert current-year)))
   (progn
     (goto-char (match-end 6))
     (insert ", ")
     (insert current-year))) t))
    (message "Copyright updated to include %s." current-year)
    (if (re-search-forward 
  "; either version \\(.+\\), or (at your option)"
  nil t)
        (progn
   (goto-char (match-beginning 1))
   (delete-region (point) (match-end 1))
   (insert current-gpl-version)))))))

(setq write-file-hooks (cons 'update-copyright-with-queries write-file-hooks))

;; Load saved Emacs session
;;(desktop-load-default)
;;(desktop-read)
