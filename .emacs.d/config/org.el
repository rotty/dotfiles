(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files '("~/org/DiplomArbeit.org"))
(setq org-CUA-compatible t)

(setq org-publish-project-alist
      '(("irclogs"
	 :base-directory "~/src/spe/systems/irclogs/docs"
	 :publishing-directory "~/src/spe/systems/irclogs/static"
	 :section-numbers nil
	 :table-of-contents nil
	 :style "<link rel=\"stylesheet\" href=\"common.css\" type=\"text/css\" />
                 <link rel=\"stylesheet\" href=\"docs.css\" type=\"text/css\" />")))
