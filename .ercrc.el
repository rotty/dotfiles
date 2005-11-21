(defun read-erc-passwords ()
  (save-excursion
    (let ((buffer (find-file-noselect "~/.erc-passwords")))
      (set-buffer buffer)
      (goto-char (point-min))
      (read buffer))))

(add-hook 'erc-after-connect
    	  '(lambda (server nick)
    	     (dolist (entry (read-erc-passwords))
	       (cond ((string-match (car entry) server)
		      (erc-message "PRIVMSG" (concat "NickServ identify "
						     (cadr entry))))))))

(require 'erc-autojoin)
(erc-autojoin-mode nil)
(setq erc-autojoin-channels-alist
      '(("irc.freenode.net" "#debian-devel" "#scheme" "#guile" "#conjure" 
	 "#scsh" "#ideologies")
	("irc.gnome.org" "#gtk+")
	("irc.oftc.net" "#debian-devel")))

(require 'erc-match)
(setq erc-keywords '("rotty"))
(erc-match-mode)

(require 'erc-ring)
(erc-ring-mode t)

(require 'erc-netsplit)
(erc-netsplit-mode t)

(erc-timestamp-mode t)
(setq erc-timestamp-format "[%R-%m/%d]")

(setq erc-auto-query 'window-noselect)

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
     "#scheme" "#guile" "#conjure" "#scsh" "#ideologies")
    (("irc.gnome.org") "#gtk+")
    (("irc.oftc.net") "#debian-devel" "#debian.or.at"))