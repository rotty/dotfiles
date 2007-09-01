;;; -*-Mode: emacs-lisp; -*-


;;; ******************
;;; Mailcrypt
;;;
(require 'mailcrypt-init)

(mc-setversion "gpg")
(setq mc-gpg-user-id "a.rottmann@gmx.at")

;;(set-keyboard-coding-system 'iso-8859-15)

;;(autoload 'mc-install-write-mode "mailcrypt" nil t)
;;(autoload 'mc-install-read-mode "mailcrypt" nil t)
;;(add-hook 'mail-mode-hook 'mc-install-write-mode)

(add-hook 'gnus-summary-mode-hook 'mc-install-read-mode)
(add-hook 'message-mode-hook 'mc-install-write-mode)
(add-hook 'news-reply-mode-hook 'mc-install-write-mode)

(defun my-gnus-start-hook ()
  (let* ((fname (expand-file-name "~/.emacs-mail-crash-box"))
	 (finfo (file-attributes fname))
	 (fsize (cond (finfo (nth 7 finfo)) (t -1))))
    (if (or (= fsize 0) (= fsize 1))
	(delete-file fname))))


;;(add-hook 'gnus-before-startup-hook 'my-gnus-start-hook)

;;(setq mail-sources '((file :path "/var/spool/mail/rotty")))

(setq mail-source-delete-incoming t) ;; get rid of Incoming* files

;; The next bunch of declarations is for not CCing me myself

(require 'bbdb-com)
(defvar my-mail-addresses (bbdb-record-net 
			   (car (bbdb-search (bbdb-records) 
					     "Andreas Rottmann"))))

(defun email-match (addr str)
  (let ((lower-str (downcase str)))
    (or (equal addr lower-str) 
	(string-match (concat "^.*<" addr ">$") lower-str))))

(defun member-email (addr list)
  (if list
      (if (email-match (car list) addr)
	  t
	(member-email addr (cdr list)))
    nil))

(defun strip-email-addresses (addrstr striplist)
  (let ((addrlist (split-string addrstr ","))
	(result ""))
    (dolist (addr addrlist result)
      (unless (member-email addr striplist) 
	(if (= (length result) 0)
	    (setq result addr)
	  (setq result (concat result "," addr)))))))

(defun my-message-reply-to-func (wide)
  (let ((orig-follow-to (message-get-reply-headers wide))
	follow-to cc-hdr)
    (dolist (hdr orig-follow-to follow-to)
      (message "calling strip")
      (let ((stripped-addrstr 
	     (strip-email-addresses (cdr hdr) my-mail-addresses)))
	(if (not (string-match stripped-addrstr "\([[:space:]]*,\)*"))
	    (setq follow-to (cons 
			     (cons (car hdr) stripped-addrstr)
			     follow-to)))))
    (setq follow-to (reverse follow-to))
    (if (assq 'To follow-to)
	follow-to
      ;; No 'To' header - take a 'Cc' and change it to 'To'
      (setq cc-hdr (assq 'Cc follow-to))
      (if cc-hdr
	  (setcar cc-hdr 'To))
      follow-to)))

(defun my-message-wide-reply-to-function ()
  (my-message-reply-to-func t))

(defun my-message-reply-to-function ()
  (my-message-reply-to-func nil))

(setq nnmail-use-long-file-names t)

;; Prefer text over HTML/RTF if available
(eval-after-load "mm-decode"
  '(progn
     (add-to-list 'mm-discouraged-alternatives "text/html")
     (add-to-list 'mm-discouraged-alternatives "text/richtext")))

;; Mailing lists (Mail-Followup-To)
(setq message-subscribed-address-functions
      '(gnus-find-subscribed-addresses))

;; Gnus methods
(setq gnus-select-method '(nntp "localhost"
				(nntp-port-number 10119)))
(setq gnus-secondary-select-methods '((nnimap "localhost"
					      (nnimap-stream ssl)
					      (nnimap-server-port 993))))

(defun fancy-split-spamassassin ()
   (save-excursion
     (set-buffer " *nnmail incoming*")
     (call-process-region (point-min) (point-max) "spamc" t t nil "-f")
     (goto-char (point-min))
     (when (re-search-forward "^x-spam-flag: yes$" nil t)
       "spam")))

(setq gnus-gcc-mark-as-read t)

;; Auto-expire all archived mailing lists
(setq gnus-auto-expirable-newsgroups
      (concat "debian-.*\\|"
	      "libsigc\\|"
	      "spi-private\\|"
	      "scheme\\..*\\|"
	      "mit-scheme\\|"
	      "gnome-announce\\|"
	      "spam"))

;; Spam
(setq spam-use-bogofilter t)
(require 'spam)

(setq gnus-spam-process-newsgroups
      '(("^nnimap+localhost:INBOX$"
	 ((spam spam-use-bogofilter)
	  (ham spam-use-bogofilter)))))

(setq gnus-message-archive-method
      '(nnfolder "archive"
		 (nnfolder-inhibit-expiry t)
		 (nnfolder-active-file "~/Mail/archive/active")
		 (nnfolder-directory "~/Mail/archive/")))

(setq gnus-message-archive-group "sent")

;; Keep GNUS from adding attribution header - we use Supercite
;;===============================
;;(setq news-reply-header-hook nil)

;; Topic mode rocks
;; ===============
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Higher Scoring of followups to myself
;;================================
(add-hook 'message-sent-hook 'gnus-score-followup-article)
(add-hook 'message-sent-hook 'gnus-score-followup-thread)

;; Signature; maybe should use different ones for different occassions
(setq my-full-signature
      (concat
       "Andreas Rottmann         | Rotty@ICQ      | 118634484@ICQ | a.rottmann@gmx.at\n"
       "http://rotty.uttx.net    | GnuPG Key: http://rotty.uttx.net/gpg.asc\n"
       "Fingerprint              | C38A 39C5 16D7 B69F 33A3  6993 22C8 27F7 35A9 92E7\n"
       "v2sw7MYChw5pr5OFma7u7Lw2m5g/l7Di6e6t5BSb7en6g3/5HZa2Xs6MSr1/2p7 hackerkey.com\n"
       ))


(setq my-quotes
      '("Python is executable pseudocode, Perl is executable line-noise."
	"It's *GNU*/Linux dammit!"
	"Anonymous surfing? Use Tor: http://tor.eff.net"
	"Make free software, not war!"
	"Latein ist das humanoide Äquivalent zu Fortran.\n   -- Alexander Bartolich in at.linux"
	"Say NO to Software Patents! -- http://petition.eurolinux.org/"
	"Any technology not indistinguishable from magic is insufficiently advanced.\n   -- Terry Pratchett"
	"The best way to accelerate a Windows machine is at 9.81 m/s^2"
	"Beware of bugs in the above code; I have only proved it correct,\nnot tried it.  -- Donald E. Knuth"
	;;"If atheism is a religion, then health is a disease. -- Clark Adams"
	"A. Because it breaks the logical sequence of discussion\nQ. Why is top posting bad?"
	"To iterate is human; to recurse, divine."
	"I have a truly elegant proof of the  above, but it is too long to\nfit into this .signature file."
	"Life is a sexually transmitted disease."
	"Software Patents: Where do you want to stifle innovation today?"
	"Always be wary of the Software Engineer who carries a screwdriver.\n  -- Robert Paul"
	"Could Jesus microwave a burrito so hot that he himself couldn't eat it? - Homer S."
	"09 f9 11 02 9d 74 e3 5b d8 41 56 c5 63 56 88 c0"))

(defun my-quote-signature ()
  (let* ((rnd (random (length my-quotes)))
	 (quote (elt my-quotes rnd)))
    (concat my-full-signature "\n" quote "\n")))

(setq message-signature 'my-quote-signature)

(setq gnus-posting-styles
      '((".*"
	 (address "a.rottmann@gmx.at")
	 (signature my-quote-signature)
	 )))

;; BBDB
(add-hook 'message-setup-hook 'bbdb-define-all-aliases)

;; GNUS Bonus
;;(require 'message-x)

;; from debbugs-el
;;(require 'gnus-BTS)

;; Mailcrypt

(add-hook 'gnus-summary-mode-hook 'mc-install-read-mode)
(add-hook 'message-mode-hook 'mc-install-write-mode)
(add-hook 'news-reply-mode-hook 'mc-install-write-mode)

;; Sucking duplicates
(setq gnus-suppress-duplicates t)

;; nnshimbun
(autoload 'gnus-group-make-shimbun-group "nnshimbun" nil t)

(setq gnus-visible-headers
      '("^From:"
	"^To:"
	"^Cc:"
	"^Followup-To:"
	"^Reply-To:"
	"^Subject:"
	"^Date:"
	"^Newsgroups:"
	"^Organization:"
	"^X-Mailer:"
	"^X-Newsreader:"
	"^X-Posting-Agent:"
	"^User-Agent:"
	"^X-Mailer:"
	"^X-MimeOLE:"))


;; Report SPAM to spamassassin
    
(defun gnus-spamassassin-report ()
  "Report message on current summary line to spamassassin."
  (interactive)
  (let* ((article 	(gnus-summary-article-number))
	 (header 	(gnus-summary-article-header article))
	 (subject 	(mail-header-subject header))
	 (from 		(mail-header-from header))
	 (buffer (save-excursion (nnheader-set-temp-buffer
				  " *spamassassin*"))))

    (if (not (vectorp header))
	(message "%s is not a real article." article)
      (when (y-or-n-p (format "Really report \"%s\" from %s to spamassassin? "
			      subject from))
	(save-window-excursion
	  (let ((gnus-display-mime-function nil)
		(gnus-article-prepare-hook nil))
	    (gnus-summary-select-article t nil nil article)))
	(save-excursion
	  (set-buffer buffer)
	  (erase-buffer)
	  (insert-buffer-substring gnus-original-article-buffer)
	  ;; Now remove Gnus' inserted headers
	  (let ((in-header 't))
	    (goto-char (point-min))
	    (while in-header
	      (dolist (field gnus-spamassassin-header-field-delete-list)
		(cond ((looking-at (concat "^" field ": "))
		       (let ((beg (point)))
			 (forward-line 1)
			 (delete-region beg (point))))
		      ((looking-at "^$")
		       (setq in-header nil))
		      ('t (forward-line 1)))))
	    (goto-char (point-max)))
	  (message "running spamassassin -r ...")
	  (let ((spamassassin (start-process "spamassassin" nil
					     "spamassassin" "-r")))
	    (process-send-region spamassassin (point-min) (point-max))
	    (process-send-eof spamassassin))
	  (run-hooks 'gnus-spamassassin-report-hook))
	(let ((del nil))
	  (if gnus-spamassassin-remove-reported-articles
	      (if (and (eq gnus-spamassassin-remove-reported-articles 'ask) 
		       (y-or-n-p "Delete this article forever? "))
		  (setq del 't)
		(setq del 't)))
	  (if (and del 
		   (gnus-check-backend-function 'request-expire-articles
						gnus-newsgroup-name))
	      (let ((not-deleted 
		     (gnus-request-expire-articles (list article)
						   gnus-newsgroup-name
						   'force)))
		(gnus-summary-remove-process-mark article)
		(unless (memq article not-deleted)
		  (gnus-summary-mark-article article gnus-canceled-mark))
		(message "Article reported as spam and removed."))
	    (message "Article reported as spam.")))))
    (gnus-kill-buffer buffer)
    (gnus-summary-position-point)
    (gnus-set-mode-line 'summary)))

(defvar gnus-spamassassin-header-field-delete-list
  '("X-Gnus-[[:alnum:]-]+"
    "Xref")
  "*List of regular expressions that match field names to be 
removed before the mail is passed to spamassassin")

(defvar gnus-spamassassin-remove-reported-articles nil)

(defun gnus-spamassassin-run-sa-learn ()
  (message "running sa-learn...")
  (let ((sa-learn 
	 (start-process "sa-learn" nil
			"sa-learn" "--spam" "--single")))
    (process-send-region sa-learn (point-min) (point-max))
    (process-send-eof sa-learn)))

(defvar gnus-spamassassin-move-mail-to-group
  "missed-spam")

(defun gnus-spamassassin-move-mail ()
  ;;(gnus-summary-mark-as-read-forward 1)
  (gnus-summary-move-article nil gnus-spamassassin-move-mail-to-group))

(add-hook 'gnus-spamassassin-report-hook 'gnus-spamassassin-move-mail)

;; If nil, sa-learn will not be run. If a function, it will be run and
;; should invoke sa-learn (perhaps on another host). Otherwise,
;; sa-learn will be run on local host
(defun gnus-spamassassin-run-sa-learn ()
  (message "running sa-learn remotely")
  (let ((sa-learn 
	 (start-process "ssh" nil
			"mail" "sudo -u mail sa-learn")))
    (process-send-region sa-learn (point-min) (point-max))
    (process-send-eof sa-learn)))


;;
;; DDTC support
;;
(defun ddtc-parse ()
  (interactive)
  (let* ((article 	(gnus-summary-article-number))
	 (header 	(gnus-summary-article-header article))
	 (subject 	(mail-header-subject header))
	 (from 		(mail-header-from header))
	 (outbuf 	(generate-new-buffer "*ddtc-output*"))
	 (inbuf (save-excursion (nnheader-set-temp-buffer
				 " *ddtc-input*"))))

    (if (not (vectorp header))
	(message "%s is not a real article." article)
      (save-window-excursion
	(let ((gnus-display-mime-function nil)
	      (gnus-article-prepare-hook nil))
	  (gnus-summary-select-article t nil nil article)))
      (save-excursion
	(set-buffer inbuf)
	(erase-buffer)
	(insert-buffer-substring gnus-original-article-buffer)
	(call-process-region (point-min) (point-max) "ddtc"
			     nil outbuf nil "parse")
	(set-buffer outbuf)
	;; TODO: Parse output and open file
	))
    (kill-buffer outbuf)
    (gnus-kill-buffer inbuf)
    (gnus-summary-position-point)
    (gnus-set-mode-line 'summary)))

;; keybindings for summary mode
(add-hook 'gnus-summary-mode-hook
	  (lambda ()
	    (define-key gnus-summary-mode-map "$" 'gnus-spamassassin-report)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use w3m instead of w3 for html mail
;;
(require 'w3m)

(defvar gnus-w3m-minor-mode nil)

(make-variable-buffer-local 'gnus-w3m-minor-mode)
(add-to-list 'minor-mode-alist '(gnus-w3m-minor-mode " w3m"))
(add-to-list 'minor-mode-map-alist (cons 'gnus-w3m-minor-mode w3m-mode-map))

(defadvice mm-inline-text (around use-w3m-instead (handle) activate)
  (let ((type (mm-handle-media-subtype handle)))
    (if (not (equal type "html"))
	ad-do-it
      (let ((text (mm-get-part handle))
	    (b (point)))
	(save-excursion
	  (insert text)
	  (save-restriction
	    (narrow-to-region b (point))
	    (goto-char (point-min))
	    (w3m-region (point-min) (point-max))
	    (setq gnus-w3m-minor-mode t))
	  (mm-handle-set-undisplayer
	   handle
	   `(lambda ()
	      (let (buffer-read-only)
		(setq gnus-w3m-minor-mode nil)
		(if (functionp 'remove-specifier)
		    (mapcar (lambda (prop)
			      (remove-specifier
			       (face-property 'default prop)
			       (current-buffer)))
			    '(background background-pixmap foreground)))
		(delete-region ,(point-min-marker)
			       ,(point-max-marker))))))))))

;; Date: Fri, 27 Jul 2001 12:51:12 +0900
;; Message-ID: <yosuwv4v3u8f.fsf@jpl.org>
;; From: Katsumi Yamaoka <yamaoka@jpl.org>
;; To: semi-gnus-ja@meadowy.org
;; Subject: [nnshimbun] toggle inline images

;; Browse image in multipart/related in koizumi mail-mag.

(eval-after-load "gnus-art"
  '(or (assoc "multipart/related" gnus-mime-multipart-functions)
       (setq gnus-mime-multipart-functions
	     (cons
	      (cons
	       "multipart/related"
	       (byte-compile
		(lambda (handle)
		  (gnus-mime-display-mixed (cdr handle)))))
	      gnus-mime-multipart-functions))))


;;----------- from Kai.Grossjohann@CS.Uni-Dortmund.DE (Kai Großjohann)

;; This function returns a string containing white space if the
;; message size is lesser than 5Kb, and the size in Kb or Mb if it is
;; greater.

(defsubst kai-gnus-summary-line-message-size (header)
  (let ((c (or (mail-header-chars header) 0)))
    (cond ((< c 5120) "     ")
	  ((< c 1048576) (format "%4dK" (/ c 1024.0)))
	  (t (format "%4dM" (/ c 1048576.0))))))

;; This add a %k field usable for the gnus-summary-line (the size
;; field as defined above)
;;(add-to-list 'gnus-summary-line-format-alist
;;             '(?k (kai-gnus-summary-line-message-size gnus-tmp-header) ?s))

(defun gnus-article-sort-by-chars (h1 h2)
  "Sort articles by size."
  (< (mail-header-chars h1)
     (mail-header-chars h2)))