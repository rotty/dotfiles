;; ********************
;; Load the auto-save.el package, which lets you put all of your autosave
;; files in one place, instead of scattering them around the file system.

(setq auto-save-directory (expand-file-name "~/autosave/")
     auto-save-directory-fallback auto-save-directory
     auto-save-hash-p nil
     efs-auto-save t
     efs-auto-save-remotely nil
     ;; now that we have auto-save-timeout, let's crank this up
     ;; for better interactive response.
     auto-save-interval 2000
     )

;; We load this afterwards because it checks to make sure the
;; auto-save-directory exists (creating it if not) when it's loaded.
(require 'auto-save)
