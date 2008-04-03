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

;; Enhanced window switching
(windmove-default-keybindings)

;; Syntax coloring
(global-font-lock-mode t)

;; Paren matching
(show-paren-mode t)

(setq european-calender-style t)
(setq diary-file "~/.diary.planner")

(setq custom-file "~/.emacs.d/.customized")
(load-file (expand-file-name custom-file))

(setq user-mail-address "a.rottmann@gmx.at")

(add-hook 'mail-setup-hook 'mail-abbrevs-setup)
(define-key emacs-lisp-mode-map "\C-xx" 'edebug-defun)

;; Extend load-path
(let ((top (expand-file-name "~/.emacs.d/lisp")))
  (add-to-list 'load-path top)
  (dolist (f (directory-files top t))
    (cond ((file-directory-p f)
	   (add-to-list 'load-path f)))))

(defun browse-url-firefox-new-tab (url &optional new-window)
  "Open URL in a new tab in Firefox."
  (interactive (browse-url-interactive-arg "URL: "))
  (if (string= (substring url 0 1) "/")
      (setq url (concat "file://" url)))
  (let ((cmd (shell-command-to-string
	      (concat "/usr/lib/firefox/firefox-xremote-client -a any 'openURL("
		      url ",new-tab)'"))))
    (unless (string= "" cmd)
      (message "Starting Firefox...")
      (start-process (concat "firefox " url) nil "firefox" url)
      (message "Starting Firefox...done"))))

(setq browse-url-browser-function 'browse-url-firefox-new-tab)

(setq debian-changelog-mailing-address "rotty@debian.org")

(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

(require 'server)
(defun gnus/server ()
  (setq server-name "gnus")
  (server-start)
  (gnus))

(load "~/.emacs.d/config-snippets.el")

(dolist (snippet config-snippets)
  (load-file (expand-file-name (concat "~/.emacs.d/config/" (symbol-name snippet) ".el"))))

;; Workaround for emacs22, see
;; http://groups.google.com/group/gnu.emacs.help/msg/d6237fdac86a7634
(provide 'sb-info)
