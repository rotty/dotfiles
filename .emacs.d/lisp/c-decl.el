(defun build-declarations ()
  "Build extern declarations for all of the functions in a file.

If invoked in the buffer for file <F>.c, this command makes changes to
the file <F>.h.  It inserts automatically generated extern
declarations in <F>.h for all functions exported from <F>.c.
It inserts these declarations near the bottom of <F>.h, prior
to the last line ending with \"#endif\", and after the last line
containing \"__STDC__\".

This funtion expects function definitions to use ANSI-style
parameter declarations.  Each definition should begin with
the return type on one line, and the function name at the beginning
of the next line.  Parameters can take any number of lines.
The first `{' in the function should be in column 0.

	int
	frob (int argc,
 	      char *argv)
	{
	}

If the type begins with the word `static' or `extern', the function is
ignored -- no declaration is generated for that function.
"
  (interactive)
  (let ((proto-buf (get-buffer-create "*protos*"))
	(source-dir default-directory))
    (save-excursion
      (set-buffer proto-buf)
      (setq default-directory source-dir)
      (delete-region (point-min) (point-max)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[a-z][^\n[=(]*(" nil t)
	(if (save-excursion
	      (and (not (looking-at ".*;.*"))
		   (re-search-forward ")[ \t]**;?$" nil t)
		   (progn
		     (forward-line 1)
		     (looking-at "{"))))
	    (progn
	      (beginning-of-line 0)
	      (if (or (looking-at "extern") (looking-at "static"))
		  (forward-line 2)
		(let ((start (point)))
		  (re-search-forward "^{" nil t)
		  (backward-char 2)
		  (let ((str (buffer-substring start (point))))
		    (save-excursion
		      (set-buffer proto-buf)
		      (goto-char (point-max))
		      (let ((p (point)))
			(insert "extern " str ";\n")
			(goto-char p)
			(replace-regexp "[ \t]*\n\\([^ \t\n]\\)" " \\1"))))))))))
    (let ((protos
	   (save-excursion
	     (set-buffer proto-buf)
	     (buffer-substring (point-min) (point-max)))))
      (let ((f1 (concat (file-name-nonextension (buffer-file-name)) ".h"))
            (f2 (concat (file-name-nonextension (buffer-file-name)) ".t")))
        (find-file (if (file-exists-p f2)
                       f2
                     f1)))
      (goto-char (point-max))
      (search-backward "__STDC__")
      (beginning-of-line 1)
      (let ((begin (point))
	    (del-endif (looking-at "#")))
	(re-search-forward "#endif.*\n")
	(beginning-of-line 1)
	(if (not del-endif)
	    (forward-line -1))
	(delete-region begin (point))
	(insert "/* automatically generated __STDC__ prototypes */\n")
	(insert protos)
	(indent-region begin (point) nil)
	(goto-char begin)))))

(defun static-declarations ()
  "Build forward declarations for all of the static functions in a file.

This command inserts automatically generated forward declarations for
all static functions in a file.  It inserts these declarations after
the first line in the file containing \"__STDC__ prototypes for static
functions\", deleting all other text from the buffer up to the next
form-feed character.

This funtion expects function definitions to use ANSI-style parameter
declarations.  Each definition should begin with the the keyword
\"static\" and the function's return type on one line, and the
function name at the beginning of the next line.  Parameters can take
any number of lines.  The first `{' in the function should be in
column 0.

	static int
	frob (int argc,
 	      char *argv)
	{
	}
"
  (interactive)
  (let ((proto-buf (get-buffer-create "*protos*"))
	(source-dir default-directory))
    (save-excursion
      (set-buffer proto-buf)
      (setq default-directory source-dir)
      (delete-region (point-min) (point-max)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[a-z][^\n[=(]*(" nil t)
	(if (save-excursion
	      (and (not (looking-at ".*;.*"))
		   (re-search-forward ")[ \t]**;?$" nil t)
		   (progn
		     (forward-line 1)
		     (looking-at "{"))))
	    (progn
	      (beginning-of-line 0)
	      (if (not (looking-at "static"))
		  (forward-line 2)
		(let ((start (point)))
		  (re-search-forward "^{" nil t)
		  (backward-char 2)
		  (let ((str (buffer-substring start (point))))
		    (save-excursion
		      (set-buffer proto-buf)
		      (goto-char (point-max))
		      (let ((p (point)))
			(insert str ";\n")
			(goto-char p)
			(replace-regexp "[ \t]*\n\\([^ \t\n]\\)" " \\1"))))))))))
    (let ((protos
	   (save-excursion
	     (set-buffer proto-buf)
	     (buffer-substring (point-min) (point-max)))))
      (goto-char (point-min))
      (search-forward "__STDC__ prototypes for static functions")
      (beginning-of-line 2)
      (insert "\n\n\n")
      (let ((begin (point)))
	(search-forward "")
	(beginning-of-line 1)
	(delete-region begin (point))
	(insert protos "\n")
	(indent-region begin (point) nil)
	(goto-char begin)))))

(defun file-name-nonextension (f)
  (substring f
	     0
	     (or (string-match "\\.[^.]*$" f)
		 (length f))))

(provide 'c-decl)
