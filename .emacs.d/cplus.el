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
			    (file-name-directory buffer-file-name)) 0 -1))))

(require 'c-decl)
