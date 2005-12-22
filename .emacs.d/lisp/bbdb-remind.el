(require 'bbdb)

(defcustom bbdb-remind-file "~/.reminders.bbdb"
  "Where reminders extracted from the BBDB are stored")

(defun bbdb-remind-export ()
  "Export BBDB database to remind file"
  (interactive)
  (save-excursion
    (bbdb ".*" nil)
    (let ((records (bbdb-records))
	  (reminders (find-file-noselect bbdb-remind-file)))
      (set-buffer reminders)
      (kill-region (point-min) (point-max))
      (while records
	(let* ((record (car records))
	       (name (bbdb-record-name record))
	       (notes (bbdb-record-raw-notes record))
	       (birthday (assoc 'birthday notes)))
	  (if birthday
	      (let ((dateparts (split-string (cdr birthday))))
		(insert "REM " (nth 0 dateparts) " " (nth 1 dateparts)
			" MSG " name "'s birthday\n")))
	  (setq records (cdr records))))
      (save-buffer))))

(provide 'bbdb-remind)
