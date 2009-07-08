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
(global-set-key [f12] 'goto-char)

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

(which-function-mode 1)

(let ((cf (getenv "EMACS_CUSTOM_FILE")))
  (if cf
      (setq custom-file cf)
    (setq custom-file "~/.emacs.d/custom/default.el")))
(when (file-readable-p custom-file)
  (load-file custom-file))

(setq user-mail-address "a.rottmann@gmx.at")

(add-hook 'mail-setup-hook 'mail-abbrevs-setup)
(define-key emacs-lisp-mode-map "\C-xx" 'edebug-defun)

;; woman
(setq woman-use-own-frame nil)


;; VC is mostly useless for modern DVCSs
(setq vc-handled-backends nil)

;; w3m-emacs's default keybinding is *horrible*
(setq w3m-key-binding 'info)

;; X11 clipboard interaction (http://www.emacswiki.org/emacs/CopyAndPaste)
;; (setq x-select-enable-primary t)
;; (setq x-select-enable-clipboard nil)
;; (setq mouse-drag-copy-region nil)
;; (setq select-active-regions t)
;; (global-set-key [mouse-2] 'mouse-yank-primary)

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

;; Taken from
;; http://www.emacswiki.org/emacs/JorgenSchaefersEmacsConfig, and
;; tweaked
(setq-default
  ;; we usually want a final newline...
 require-final-newline 'ask
 ;; No, please, no tabs in my programs!
 indent-tabs-mode nil
 ;; I don't like emacs destroying my window setup
 even-window-heights nil
 ;; Same here
 resize-mini-windows nil
 ;; No am/pm here
 display-time-24hr-format t
 ;; A tab is 8 spaces is 8 spaces is 8 spaces
 default-tab-width 8
 ;; Scrolling is moving the document, not moving my eyes
 scroll-preserve-screen-position 'keep
 ;; My email address
 user-mail-address "a.rottmann@gmx.at"
 ;; I kinda know my emacs
 inhibit-startup-message t
 ;; unified context is nicer to read
 diff-switches "-u"
 ;; A wide characters ask for a wide cursor
 x-stretch-cursor t
 ;; i want a mouse yank to be inserted where the point is, not where i click
 mouse-yank-at-point t
 ;; Don't highlight stuff that I can click on all the time.
 mouse-highlight 1
 )

;; Oh, and I want useful mouse selection.
(mouse-sel-mode 1)

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

;; support for starting gnus as a server
(require 'server)
(defun gnus/server ()
  (setq server-name "gnus")
  (server-start)
  (gnus))


;; Expansion
(global-set-key (kbd "M-/") 'hippie-expand)

(defun try-my-dabbrev-substring (old)
  (let ((old-fun (symbol-function 'he-dabbrev-search)))
    (fset 'he-dabbrev-search (symbol-function 'my-dabbrev-substring-search))
    (unwind-protect
        (try-expand-dabbrev old)
      (fset 'he-dabbrev-search old-fun))))


(defun my-dabbrev-substring-search (pattern &optional reverse limit)
  (let ((result ())
	(regpat (cond ((not hippie-expand-dabbrev-as-symbol)
		       (concat (regexp-quote pattern) "\\sw+"))
		      ((eq (char-syntax (aref pattern 0)) ?_)
		       (concat (regexp-quote pattern) "\\(\\sw\\|\\s_\\)+"))
		      (t
		       (concat (regexp-quote pattern)
			       "\\(\\sw\\|\\s_\\)+")))))
    (while (and (not result)
		(if reverse
		     (re-search-backward regpat limit t)
		     (re-search-forward regpat limit t)))
      (setq result (buffer-substring-no-properties (save-excursion
                                                     (goto-char (match-beginning 0))
                                                     (skip-syntax-backward "w_")
                                                     (point))
						   (match-end 0)))
      (if (he-string-member result he-tried-table t)
	  (setq result nil)))     ; ignore if bad prefix or already in table
    result))

(defun umlaut ()
  (interactive)
  (let ((l '(("a" . "ä")
	     ("o" . "ö")
	     ("u" . "ü")
	     ("A" . "Ä")
	     ("O" . "Ö")
	     ("U" . "Ü")
	     ("s" . "ß")
	     ("dash" . "–")
	     ("dots" . "…")
	     ("lquot" . "“")
	     ("rquot" . "”")
	     ("glquot" . "„")
	     ("grquot" . "“")
	     ("lquotsingle" . "‘")
	     ("rquotsingle" . "’")
	     ("euro" . "€")
	     ("interrobang" . "‽")
	     ("kbdalt" . "⌥")
	     ("kbdcommand" . "⌘")
	     ("kbdshift" . "⇧"))))
    (let ((u (completing-read "Short form: " l nil t)))
      (let ((r (assoc u l)))
	(when r (insert (cdr r)))))))

(eval-after-load "ispell"
  (progn
    (setq ispell-dictionary "nynorsk"
          ispell-extra-args '("-a" "-i" "utf-8") ; aspell doesn't understand -i utf-8, hunspell needs it
          ispell-silently-savep t)))
(setq-default ispell-program-name "hunspell")


;; Config snippets loading; this provides an easy way to define
;; 'configuration snippets' for use with a specific package (that may
;; be not installed), and enable loading that snippet when its
;; basename (specified as a symbol) is part of the variable
;; config-snippets.
(defgroup config-snippets nil
  "Configuration snippets -- elisp files that are loaded at the startup.")

(defcustom config-snippets '()
  "Specifies the config snippets to be loaded at startup.
Elements may be strings (interpreted as literal filenames) or
symbols (converted to as string, which is suffixed with \".el\")."
  :group 'config-snippets
  :type '(repeat (choice symbol string)))

(defcustom config-snippet-path '("~/.emacs.d/config/")
  "Specifies the path in which config snippets are searched"
  :group 'config-snippets
  :type '(repeat directory))

(dolist (snippet config-snippets)
  (dolist (dir config-snippet-path)
    (let ((file-name (expand-file-name (concat dir (if (symbolp snippet)
						       (concat "cfgsnippet-" (symbol-name snippet) ".el")
						     snippet)))))
      (if (file-readable-p file-name)
          (load-file file-name)
	(message "Config snippet not found: %s" snippet)))))


;; Workaround for emacs22, see
;; http://groups.google.com/group/gnu.emacs.help/msg/d6237fdac86a7634
(provide 'sb-info)


(defun new-term ()
  (interactive)
  (term "/bin/zsh")
  (rename-uniquely))

(defvar revert-some-buffers-action-alist
  '((?\C-r
     (lambda (buf)
       (view-buffer buf
		    (lambda (ignore)
		      (exit-recursive-edit)))
       (recursive-edit)
       ;; Return nil to ask about BUF again.
       nil)
     "view this buffer")
    (?d (lambda (buf)
	  (save-window-excursion
	    (diff-buffer-with-file buf))
	  (view-buffer (get-buffer-create "*Diff*")
		       (lambda (ignore) (exit-recursive-edit)))
	  (recursive-edit)
	  nil)
	"view changes in this buffer"))
  "ACTION-ALIST argument used in call to `map-y-or-n-p'.")

;; Code/docstring taken from Emacs' `save-some-buffers' and hacked.
(defun revert-some-buffers (&optional arg pred)
  "Revert some modified file-visiting buffers.  Asks user about each one.
You can answer `y' to revert, `n' not to revert, `C-r' to look at the
buffer in question with `view-buffer' before deciding or `d' to
view the differences using `diff-buffer-with-file'.

Optional argument (the prefix) non-nil means revert all with no questions.
Optional second argument PRED determines which buffers are considered:
If PRED is nil, all the file-visiting buffers are considered.
If PRED is t, then certain non-file buffers will also be considered.
If PRED is a zero-argument function, it indicates for each buffer whether
to consider it or not when called with that buffer current."
  (interactive "P")
  (save-window-excursion
    (let (queried files-done)
      ;; Ask about those buffers that merit it,
      ;; and record the number thus reverted.
      (setq files-done
	    (map-y-or-n-p
	     (function
	      (lambda (buffer)
		(and (not (buffer-modified-p buffer))
		     (not (buffer-base-buffer buffer))
		     (buffer-file-name buffer)
		     (not (verify-visited-file-modtime buffer))
		     (file-exists-p (buffer-file-name buffer))
		     (or (not (functionp pred))
			 (with-current-buffer buffer (funcall pred)))
		     (if arg
			 t
		       (setq queried t)
		       (format "Revert file %s? " (buffer-file-name buffer))))))
	     (function
	      (lambda (buffer)
		(set-buffer buffer)
		(revert-buffer 'ignore-auto 'dont-ask 'preserve-modes)))
	     (buffer-list)
	     '("buffer" "buffers" "save")
	     revert-some-buffers-action-alist))
      (or queried (> files-done 0)
	  (message "(No files need reverting)")))))


;; Enable desktop saving on exit; also loads the desktop on startup, so goes last
(desktop-save-mode 1)

(setq history-length 250)
(dolist (var '(file-name-history command-history))
  (add-to-list 'desktop-globals-to-save var))

;; Desktop autosave (http://www.emacswiki.org/cgi-bin/wiki/DeskTop)
(setq *foo-desktop-dir* (expand-file-name "~/.emacs.d/desktop/"))

(setq desktop-dir *foo-desktop-dir*)
(setq desktop-path (list *foo-desktop-dir*))

(setq *foo-desktop-file* (concat desktop-dir desktop-base-file-name))

(setq *foo-desktop-lock* (concat desktop-dir desktop-base-lock-name))

(defun desktop-in-use-p ()
  (and desktop-save-mode
       (file-exists-p *foo-desktop-file*)
       (file-exists-p *foo-desktop-lock*)))

(defun autosave-desktop ()
  (if (desktop-in-use-p)
      (desktop-save-in-desktop-dir)))

;; Can be switched off with (cancel-timer *foo-desktop-saver-timer*)
(add-hook 'after-init-hook
	  (lambda ()
	    (setq *foo-desktop-saver-timer* 
		  (run-with-timer 5 300 'autosave-desktop))))
