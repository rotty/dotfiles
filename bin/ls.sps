#!/usr/bin/env scheme-script

(import (except (rnrs) file-exists? delete-file)
        (spells tracing)
        (spells format)
        (spells pathname)
        (spells filesys))

(define (main argv)
  (let ((argc (length argv)))
    (if (= argc 1)
        (ls "." (current-output-port))
        (for-each (lambda (path)
                    (ls path (current-output-port)))
                  (cdr argv)))))

(define (ls path port)
  (let ((pathname (x->pathname path)))
    (if (file-directory? path)
        (directory-fold pathname
                        (lambda (pathname)
                          (format-stats port pathname)
                          (values)))
        (format-stats port pathname))))

(define (format-stats port pathname)
  (format port "~A ~A~%" (file-namestring pathname) (file-size-in-bytes pathname)))

(main (command-line))
