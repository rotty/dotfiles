;; -*- Mode: Emacs-Lisp -*-

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
(setq global-font-lock-mode t)

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


(require 'dired)

;; Extend load-path
(push (expand-file-name "~/.emacs.d/lisp") load-path)


(dolist (snippet '("scheme"))
  (load (expand-file-name (concat"~/.emacs.d/config/" snippet ".el"))))

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


;; CSS-Mode
;;
(setq cssm-indent-function #'cssm-c-style-indenter)

;; PSGML
(setq sgml-set-face t)
(setq sgml-indent-step 1)

;; Tuareg-Mode (OCAML)
(autoload 'tuareg-mode "tuareg" nil t)


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

(require 'bbdb)
(bbdb-initialize 'gnus 'message 'w3)
(require 'bbdb-remind)

;; Some goodies
(require 'follow-mouse)


;; show paren mode
(show-paren-mode t)

;; Emacs speaks statistics
;(require 'ess-site)

;; buffer stack (overwriting bindings for scroll-left and scroll-right)
(require 'buffer-stack)
(global-set-key [C-next] `buffer-stack-down)
(global-set-key [C-prior] `buffer-stack-up)

;; (autoload 'newsticker-start "newsticker" "Emacs Newsticker" t)
;; (autoload 'newsticker-show-news "newsticker" "Emacs Newsticker" t)

;; (setq newsticker-url-list
;;       '(("Planet Scheme" "http://planet-scheme.yi.org/rss20.xml")))

;; GNU Arch
;;(require 'xtla)

;;
;; emacs-goodies-el
;;

;; dirvars
(require 'dirvars)

;; projects (nicer buffer names)
;;(require 'projects)

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
