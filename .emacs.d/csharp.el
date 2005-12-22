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


