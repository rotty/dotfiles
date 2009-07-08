(require 'java-mode-indent-annotations)

(defun my-java-mode-hook ()
  (c-set-offset 'inclass '+)
  (java-mode-indent-annotations-setup))

(add-hook 'java-mode-hook 'my-java-mode-hook)

;; JDE
;;;
;;(require 'jde)
;;(require 'jde-ant)
