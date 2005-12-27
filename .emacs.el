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

(iswitchb-mode t)
(global-font-lock-mode t)

;; I do everything manually
(setq custom-file nil)

;; Extend load-path
(let ((top (expand-file-name "~/.emacs.d/lisp")))
  (add-to-list 'load-path top)
  (dolist (f (directory-files top t))
    (cond ((file-directory-p f)
	   (add-to-list 'load-path f)))))

(dolist (snippet '("scheme" "cplus" "slime48" "tramp" "crypt"))
  (load (expand-file-name (concat"~/.emacs.d/config/" snippet ".el"))))

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

;; show paren mode
(show-paren-mode t)

;; (autoload 'newsticker-start "newsticker" "Emacs Newsticker" t)
;; (autoload 'newsticker-show-news "newsticker" "Emacs Newsticker" t)

;; (setq newsticker-url-list
;;       '(("Planet Scheme" "http://planet-scheme.yi.org/rss20.xml")))

;; Diary
(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'list-diary-entries-hook 'sort-diary-entries)
(add-hook 'list-diary-entries-hook 'include-other-diary-files)
(add-hook 'mark-diary-entries-hook 'mark-included-diary-files)



(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Use UTF-8 for file name encoding
(setq file-name-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Load saved Emacs session
;;(desktop-load-default)
;;(desktop-read)
