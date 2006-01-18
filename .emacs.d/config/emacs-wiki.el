(require 'emacs-wiki)
(require 'cl)

(setq emacs-wiki-style-sheet 
;; We need to hack around broken IE
      (concat
       "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />\n"
       "<!--[if gte IE 5]>\n"
       "  <link rel=\"stylesheet\" type=\"text/css\" href=\"style-ie-overrides.css\" />\n"
       "<![endif]-->"))

(setq emacs-wiki-maintainer "mailto:a.rottmann@gmx.at")
(setq emacs-wiki-coding-default 'utf-8)
(setq emacs-wiki-charset-default "utf-8")

(defun rotty/emacs-wiki-auto-publish ()
  (unless emacs-wiki-publishing-p
    (let ((emacs-wiki-publishing-p t))
      (emacs-wiki-publish-this-page))))

;; (add-hook 'emacs-wiki-mode-hook 
;; 	  (lambda () 
;; 	    (add-hook 'after-save-hook 'rotty/emacs-wiki-auto-publish nil t)))

(setq emacs-wiki-file-ignore-regexp
  "\\`\\(\\.html\\|\\..*\\|\\.?#.*\\|.*,v\\|.*~\\|\\.\\.?\\)\\'")

(defun emacs-wiki-edit-link-at-point ()
  "Edits the current link but does not rename the wiki page originally referred to."
  (interactive "*")
  (let (old-name)
    (if (emacs-wiki-link-at-point)
        (replace-match (save-match-data (read-string "Edit link: " (match-string-no-properties 0))))
      (error "There is no valid link at point"))))

(define-key emacs-wiki-mode-map (kbd "C-c C-e") 'emacs-wiki-edit-link-at-point)


(setq emacs-wiki-projects
      '(("rotty"
	 (emacs-wiki-publishing-directory . "~/www/rotty")
	 (emacs-wiki-directories "~/wiki/rotty")
	 (emacs-wiki-publishing-header . "<lisp>(insert-file \"~/wiki/rotty/.header\")</lisp>")
         (emacs-wiki-publishing-footer . "<lisp>(insert-file \"~/wiki/rotty/.footer\")</lisp>")
	 )
	("home"
	 (emacs-wiki-publishing-directory . "~/public_html")
	 (emacs-wiki-directories "~/wiki/home")
	 (emacs-wiki-publishing-header . "<lisp>(insert-file \"~/wiki/home/.header\")</lisp>")
         (emacs-wiki-publishing-footer . "<lisp>(insert-file \"~/wiki/home/.footer\")</lisp>")
	 )
	
	("conjure"
	 (emacs-wiki-publishing-directory . "~/src/contrib/conjure/+www")
	 (emacs-wiki-directories "~/src/contrib/conjure/wiki/pages")
	 ;;(emacs-wiki-publishing-header . "<lisp>(insert-file \"~/src/contrib/g-wrap/+wiki/.header\")</lisp>")
         ;;(emacs-wiki-publishing-footer . "<lisp>(insert-file \"~/src/contrib/g-wrap/+wiki/.footer\")</lisp>")
	 )

	("g-wrap"
	 (emacs-wiki-publishing-directory . "~/src/contrib/g-wrap/+www")
	 (emacs-wiki-directories "~/src/contrib/g-wrap/+wiki")
	 (emacs-wiki-publishing-header . "<lisp>(insert-file \"~/src/contrib/g-wrap/+wiki/.header\")</lisp>")
         (emacs-wiki-publishing-footer . "<lisp>(insert-file \"~/src/contrib/g-wrap/+wiki/.footer\")</lisp>")
	 )

	("guile-lib"
	 (emacs-wiki-publishing-directory . "~/src/contrib/guile-lib/+www")
	 (emacs-wiki-directories "~/src/contrib/guile-lib/+wiki")
	 (emacs-wiki-publishing-header . "<lisp>(insert-file \"~/src/contrib/guile-lib/+wiki/.header\")</lisp>")
         (emacs-wiki-publishing-footer . "<lisp>(insert-file \"~/src/contrib/guile-lib/+wiki/.footer\")</lisp>")
	 )))


(setq emacs-wiki-charset-default "utf-8")

(setq emacs-wiki-publishing-directory "~/public_html/")
(setq emacs-wiki-table-attributes "border=\"0\" cellpadding=\"2\" cellspacing=\"0\"")

(dolist (elt '(("question" t nil nil emacs-wiki-question-tag)
	       ("hint" t nil nil emacs-wiki-hint-tag)
	       ("wink" nil nil nil emacs-wiki-wink-tag)
	       ("grin" nil nil nil emacs-wiki-grin-tag)
	       ("laugh" nil nil nil emacs-wiki-laugh-tag)
	       ("smile" nil nil nil emacs-wiki-smile-tag)))
  (add-to-list 'emacs-wiki-markup-tags elt))

(defsubst emacs-wiki-link-href-menu (name)
  "Return an item for the menu with NAME. Use .menu stylesheet class for menu item. "
  (concat "<a class=\"menu\" href=\"" (emacs-wiki-published-name name) "\">" (emacs-wiki-page-title name) "</a><br />"))

(defun emacs-wiki-generate-menu-item (item &optional noclass)
  (cond ((stringp item)
	 (concat "<a " (if noclass "" "class=\"menu\" ") "href=\""
		 (emacs-wiki-published-name item) "\">" 
		 item "</a>"))
	((and (consp item) (consp (cdr item))) ; more than one item in the line
	 (concat
	  "<span class=\"menu\">"
	  (apply 'concat (map 'list 
			      (lambda (i)
				(concat (emacs-wiki-generate-menu-item i 't) 
					" "))
			      item))
	  "</span>"))
	((consp item) ; item with specified name
	 (concat "<a " (if noclass "" "class=\"menu\" ") "href=\""
		 (emacs-wiki-published-name (car item)) "\">"
		 (cdr item) "</a>"))))
		 
(defun emacs-wiki-generate-menu (menu)
  (apply 'concat 
	 (map 'list (lambda (item) 
		      (concat (emacs-wiki-generate-menu-item item) "<br />\n"))
	      menu)))

(defun emacs-wiki-question-tag (beg end))

(defun emacs-wiki-hint-tag (beg end))

(defun emacs-wiki-wink-tag (beg end)
  (insert "<img src=\"pix/face-winking.png\" alt=\";-)\" />"))

(defun emacs-wiki-grin-tag (beg end)
  (insert "&lt;grin&gt;"))

(defun emacs-wiki-laugh-tag (beg end)
  (insert "&lt;laugh&gt;"))
(defun emacs-wiki-smile-tag (beg end)
  (insert "<img src=\"pix/face-happy.png\" alt=\":-)/>\""))
