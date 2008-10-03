(require 'erc-services)

(load "~/.ercrc-local")

;; (require 'erc-join)
;; (erc-autojoin-mode nil)
;; (setq erc-autojoin-channels-alist
;;       '(("irc.freenode.net" "#debian-devel" "#scheme" "#guile" "#conjure" 
;; 	 "#scsh" )
;; 	("irc.gnome.org" "#gtk+")
;; 	("irc.oftc.net" "#debian-devel")
;; 	))

(require 'erc-match)
(setq erc-keywords '("rotty"))
(erc-match-mode)

(require 'erc-ring)
(erc-ring-mode t)
(local-set-key [C-up] 'erc-previous-command)
(local-set-key [C-down] 'erc-next-command)

(require 'erc-netsplit)
(erc-netsplit-mode t)

(require 'erc-services)
(erc-services-mode 1)

(erc-timestamp-mode t)
(setq erc-timestamp-format "[%R-%m/%d]")

(setq erc-auto-query 'window-noselect)

(setq erc-track-exclude-types '("NICK" "JOIN" "PART" "QUIT" "MODE"))

(defmacro erc-autojoin (&rest args)
  `(add-hook 'erc-after-connect
	     '(lambda (server nick)
		(cond
		 ,@(mapcar (lambda (servers+channels)
			     (let ((servers (car servers+channels))
				   (channels (cdr servers+channels)))
			       `((member erc-session-server ',servers)
				 (mapc 'erc-join-channel ',channels))))
			   args)))))

(erc-autojoin
    (("irc.freenode.net") 
     "#scheme" "#guile" "#conjure" "#scsh" "#ideologies" "#sisc")
    (("irc.gnome.org") "#gtk+" "#flow" "#vala" "#gnome-hackers")
    (("irc.oftc.net") "#debian-devel" "#debian.or.at")
    (("irc.perl.org") "#parrot"))
