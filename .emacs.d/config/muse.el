(require 'muse-mode)     ; load authoring mode
(require 'muse-html)    ; load publishing styles I use(require 'muse)

(require 'muse-project)

(setq muse-project-alist
      '(("homepage"			; my various writings
	 ("~/muse" :default "index")
	 (:base "xhtml" :path "~/public_html"))))

(setq muse-xhtml-style-sheet 
      "<link rel=\"stylesheet\" type=\"text/css\" charset\=\"utf-8\" media=\"all\" href=\"screen.css\" />")

(setq muse-xhtml-header "~/muse/.header")
(setq muse-xhtml-footer "~/muse/.footer")
