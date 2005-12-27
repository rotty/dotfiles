(require 'table)

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
