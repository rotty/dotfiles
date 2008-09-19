;; Some goodies
(require 'follow-mouse)

;; buffer stack (overwriting bindings for scroll-left and scroll-right)
(require 'buffer-stack)
(global-set-key [C-next] `buffer-stack-down)
(global-set-key [C-prior] `buffer-stack-up)

;;
;; emacs-goodies-el
;;

;; dirvars
(require 'dirvars)

;; projects (nicer buffer names)
;;(require 'projects)

;; Misc helpful functions
(load "util-funcs")

(add-hook 'write-file-hooks 'copyright-update)

(setq darcsum-diff-switches "-u")
