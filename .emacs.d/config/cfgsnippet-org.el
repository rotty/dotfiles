(setq org-replace-disputed-keys t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(setq org-agenda-files '("~/org/AGENDA"))

(defun rotty/homepage-preamble (opt-plist)
  (insert "<div id=\"header\">")
  (insert
   "
<div id=\"navbar\">
<a class=\"menu\" href=\"/\">Home</a>
| <a class=\"menu\" href=\"/Writings.html\">Writings</a>
| <a class=\"menu\" href=\"/software/\">Software</a>
| <a class=\"menu\" href=\"http://rotty.yi.org/blog/\">Blog</a>
</div>
")
  (insert (format 
           "<h1 class=\"title\"><a href=\"/\">rotty</a> &gt; %s</h1>\n"
           (plist-get opt-plist :title)))
  (insert "</div>\n")
  (insert "<div id=\"inner\">\n"))

(defun rotty/homepage-postamble (opt-plist)
  (insert
   "
</div> <!-- inner -->

<div id=\"footer\">
<hr/>

<a href=\"http://validator.w3.org/check/referer\">
<img class=\"pic\" src=\"/pix/valid-xhtml10.png\" alt=\"Valid XHTML 1.0\" />
</a>

Andreas Rottmann - <a href=\"mailto:a.rottmann@gmx.at\">a.rottmann@gmx.at</a>
<br />
Made with <a href=\"http://orgmode.org/\">Emacs Org-Mode</a>!

</div> <!-- footer -->
"))

(setq org-publish-project-alist
      `(("irclogs"
	 :base-directory "~/src/spe/systems/irclogs/docs"
	 :publishing-directory "~/src/spe/systems/irclogs/static"
	 :section-numbers nil
	 :table-of-contents nil
         :style-include-default nil
	 :style "<link rel=\"stylesheet\" href=\"common.css\" type=\"text/css\" />
                 <link rel=\"stylesheet\" href=\"docs.css\" type=\"text/css\" />")
	
	("sbank"
	 :base-directory "~/src/spe/systems/sbank/docs"
	 :base-extension "org"
	 ;;:include ("README")
	 :publishing-directory "~/src/spe/systems/sbank/docs/html"
	 :section-numbers nil
	 :table-of-contents nil)
        ("homepage"
         :base-directory "~/homepage"
         :recursive t
	 :base-extension "org"
         :publishing-directory "~/public_html"
         :section-numbers nil
         :table-of-contents nil
         :auto-preamble nil
         :auto-postamble nil
         :preamble rotty/homepage-preamble
         :postamble rotty/homepage-postamble
         :style-include-default nil
         :style 
         "<link rel=\"stylesheet\" href=\"/screen.css\" type=\"text/css\" />")))
