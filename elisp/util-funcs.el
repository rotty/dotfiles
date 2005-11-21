(defun read-file-into-string (name)
  "Return the contents of the file as a string"
  (let ((buf (generate-new-buffer "*temp*"))
	(result nil))
    (save-excursion
      (set-buffer buf)
      (insert-file-contents name)
      (setq result (buffer-substring (point-min) (point-max)))
      (kill-buffer buf))
    result))

(defmacro point-pos (position)
  ;; Returns the value of point at certain commonly referenced POSITIONs.
  ;; POSITION can be one of the following symbols:
  ;; 
  ;; bol  -- beginning of line
  ;; eol  -- end of line
  ;; bod  -- beginning of defun
  ;; boi  -- back to indentation
  ;; ionl -- indentation of next line
  ;; iopl -- indentation of previous line
  ;; bonl -- beginning of next line
  ;; bopl -- beginning of previous line
  ;; 
  ;; This function does not modify point or mark.
  (or (and (eq 'quote (car-safe position))
	   (null (cdr (cdr position))))
      (error "bad buffer position requested: %s" position))
  (setq position (nth 1 position))
  (` (let ((here (point)))
       (,@ (cond
	    ((eq position 'bol)  '((beginning-of-line)))
	    ((eq position 'eol)  '((end-of-line)))
	    ((eq position 'bod)
	     '((beginning-of-defun)
	       ;; if defun-prompt-regexp is non-nil, b-o-d won't leave
	       ;; us at the open brace.
	       (and (boundp 'defun-prompt-regexp)
		    defun-prompt-regexp
		    (looking-at defun-prompt-regexp)
		    (goto-char (match-end 0)))
	       ))
	    ((eq position 'boi)  '((back-to-indentation)))
	    ((eq position 'bonl) '((forward-line 1)))
	    ((eq position 'bopl) '((forward-line -1)))
	    ((eq position 'iopl)
	     '((forward-line -1)
	       (back-to-indentation)))
	    ((eq position 'ionl)
	     '((forward-line 1)
	       (back-to-indentation)))
	    (t (error "unknown buffer position requested: %s" position))
	    ))
       (prog1
	   (point)
	 (goto-char here))
       ;; workaround for an Emacs18 bug -- blech! Well, at least it
       ;; doesn't hurt for v19
       (,@ nil)
       )))

(defun string-equal-chars (string1 string2)
  "Compares STRING1 with STRING2, char by char. It returns the
number of matching characters."
  (let ((nmax (1- (min (length string1) (length string2))))
        (n 0))
    (catch 'exit
      (while (<= n nmax)
        (if (char-equal (aref string1 n) (aref string2 n))
            ()
          (throw 'exit n))
        (setq n (1+ n))))
    n))

(defun make-c++-header ()
  (interactive)
  (let (bufname)
    (setq bufname (upcase (buffer-name)))
    (setq bufname (substring bufname 0 (string-match "\.H$" bufname)))
    ;; '-' -> '_'
    (let* ((sz (length bufname))
	   (i 0))
      (while (< i sz)
	(if (eq (aref bufname i) ?-)
	    (aset bufname i ?_))
	(setq i (+ i 1))))
    (insert "/* This is for emacs: -*-Mode: C++;-*- */\n")
    (insert "#if !defined(_INC_" bufname "_H)\n")
    (insert "#define _INC_" bufname "_H\n\n")
    (insert "#endif")
    (forward-line -1)
    (c++-mode)
    (font-lock-mode 1)
    )
  )

(defun make-c-header ()
  (interactive)
  (let (bufname)
    (setq bufname (upcase (buffer-name)))
    (setq bufname (substring bufname 0 (string-match "\.H$" bufname)))
    (insert "#if !defined(_INC_" bufname "_H)\n")
    (insert "#define _INC_" bufname "_H\n\n")
    (insert "#ifdef __cplusplus\nextern \"C\" {\n#endif\n\n\n\n")
    (insert "#ifdef __cplusplus\n}\n#endif\n\n")
    (insert "#endif")
    (forward-line -6)
    )
  )

(defun cvs-kill-buffers ()
  (interactive)
  (let ((buflist (buffer-list))
        (bufname nil))
    (while buflist
      (setq bufname (buffer-name (car buflist)))
      (if (and (car buflist)
               (>= (length bufname) 4)
               (string= "*cvs" (substring bufname 0 4)))
          (kill-buffer (car buflist)))
      (setq buflist (cdr buflist)))))


;;
;; This substitutes any users home directory with '~user' or '~' when
;; the home directory is the current user's home directory.
;;;###autoload
(defun home-abbrev-file-name (filename)
  (interactive)
  (if (char-equal (aref filename 0) ?/)
      (let* ((buffer-read-only nil)
             (passwd-buffer (find-file-noselect "/etc/passwd"))
             (username nil)
             (n 0)
             (nmax 0))
        (save-excursion
          (set-buffer passwd-buffer)
          (goto-char (point-min))
          (while (not (eq (point) (point-pos 'bonl)))
            (if (looking-at "\\(.*\\):.*:.*:.*:.*:\\(.*\\):")
                (progn
                  (setq n (progn
                            (if (< (string-equal-chars filename
                                                       (match-string 2))
                                   (length (match-string 2)))
                                0
                              (length (match-string 2)))))
                        
                  (if (> n nmax)
                      (progn (setq nmax n)
                             (setq username (match-string 1))))))
            (forward-line 1))
          (kill-buffer passwd-buffer)
          (if username
              (progn (if (equal username (getenv "USER"))
                         (setq username ""))
                     (concat "~" username (substring filename nmax)))
            filename)
          ))))

;; Update copyright

(defconst current-year (substring (current-time-string) -4)
  "String representing the current year.")
(defconst last-year (int-to-string (- (string-to-int current-year) 1))
  "String representing the current year (presuming that the current year is not 1 AD, which hopefully will continue to be the case indefinitely).")
(defvar current-gpl-version "2"
  "String representing the current version of the GPL.")
(defvar copyright-regex "[Cc]opyright\\s *\\(([Cc])\\)?\\(\\s *[0-9]+\\s *\\(-\\s *[0-9]+\\s *\\)?,\\s *\\)*\\s *\\(\\([0-9]+\\)\\s *-\\)?\\s *\\([0-9]+\\)"
  "Regular expression to match common copyright declarations, extracting the final year(s).")
;; Note: paren expr. #5 is the first year of the last dashed pair, if
;; any; paren expr. #6 is the last year.

(defun update-copyright-with-queries ()
  "My version of update-copyright."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (and (re-search-forward "[i]s free software"
                              nil t)
           (not (eq major-mode 'rmail-mode))
           (let ((limit (point)))
             (goto-char (point-min))
             (re-search-forward copyright-regex
              limit t))
    (let ((final-year (match-string 6))
   (final-range-start (match-string 5)))
      (when (and (not (string= final-year current-year))
   (progn (goto-char (point-min))
          (sit-for 0)
          (y-or-n-p (format "Update copyright (last %s)? " final-year))))
        (if (string= final-year last-year)
     (if final-range-start
         (progn
    (goto-char (match-end 6))
    (delete-region (match-beginning 6) (match-end 6))
    (insert current-year))
       (progn
         (goto-char (match-end 6))
         (insert "-")
         (insert current-year)))
   (progn
     (goto-char (match-end 6))
     (insert ", ")
     (insert current-year))) t))
    (message "Copyright updated to include %s." current-year)
    (if (re-search-forward 
  "; either version \\(.+\\), or (at your option)"
  nil t)
        (progn
   (goto-char (match-beginning 1))
   (delete-region (point) (match-end 1))
   (insert current-gpl-version)))))))
