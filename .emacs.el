;; -*- Mode: Emacs-Lisp -*-

;; enable some commands disabled by default
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Use UTF-8 for file name encoding
(setq file-name-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; No fancy graphic stuff, you don't want to use the mouse anyway, do
;; you?
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))


;; http://opal.cabochon.com/~stevey/blog-rants/effective-emacs.html
(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key [f5]  'call-last-kbd-macro)

;; Some other requently-used functions
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)

(global-set-key [f11] 'goto-line)
(global-set-key [f12] 'toggle-transient-mark-mode)

;; Enhanced buffer switching
(iswitchb-mode t)

;; Syntax coloring
(global-font-lock-mode t)

;; Paren matching
(show-paren-mode t)

;; I do configuration manually; the customization feature just gets in
;; the way of modularizing your emacs configuation, IMHO
(setq custom-file nil)


(add-hook 'mail-setup-hook 'mail-abbrevs-setup)
(define-key emacs-lisp-mode-map "\C-xx" 'edebug-defun)

;; Extend load-path
(let ((top (expand-file-name "~/.emacs.d/lisp")))
  (add-to-list 'load-path top)
  (dolist (f (directory-files top t))
    (cond ((file-directory-p f)
	   (add-to-list 'load-path f)))))

(dolist (snippet '("scheme" "cplus" "slime48" "tramp" "crypt"))
  (load (expand-file-name (concat"~/.emacs.d/config/" snippet ".el"))))
