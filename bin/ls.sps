#!/usr/bin/env scheme-script

(import (except (rnrs) file-exists? delete-file fold-right)
        (xitomatl srfi time)
        (only (spells lists) make-list fold-right)
        (spells receive)
        (spells tracing)
        (spells pathname)
        (spells filesys)
        (fmt))

(define (main argv)
  (let ((argc (length argv)))
    (if (= argc 1)
        (ls "." (current-output-port))
        (for-each (lambda (path)
                    (ls path (current-output-port)))
                  (cdr argv)))))

(define (ls path port)
  (let* ((pathname (x->pathname path))
         (stats
          (if (file-directory? pathname)
              (directory-fold (pathname-as-directory pathname)
                              (lambda (pathname lst)
                                (cons (get-stats pathname) lst))
                              '())
              (list (get-stats pathname)))))
    (receive (columns widths) (columnify/widths stats)
      (let ((total-width (apply + widths)))
        (fmt port
             (join/suffix
              dsp
              (map (lambda (line)
                     (join dsp
                           (map (lambda (padder width item)
                                  (padder width item))
                                (reverse
                                 (cons pad (make-list (- (length widths) 1) pad/left)))
                                widths
                                line)
                           " "))
                   stats)
              nl))))))

(define (columnify/widths lines)
  (let next-line ((n-columns 0) (widths '()) (columns '()) (n-lines 0) (lines lines))
    (if (null? lines)
        (values columns widths)
        (let* ((items (map (lambda (item) (fmt #f (dsp item))) (car lines)))
               (n-items (length items))
               (new-columns (if (> n-items n-columns)
                                (append columns (make-list (- n-items n-columns)
                                                           (make-list n-lines "")))
                                columns))
               (new-items (if (> n-columns n-items)
                              (append items (make-list (- n-columns n-items) ""))
                              items))
               (new-widths (if (> n-items n-columns)
                               (append widths (make-list (- n-items n-columns) 0))
                               widths))
               (new-n-columns (max n-items n-columns)))
          (next-line new-n-columns
                     (map (lambda (item width)
                            (max (string-length item) width))
                          new-items new-widths)
                     (map cons new-items new-columns)
                     (+ n-lines 1)
                     (cdr lines))))))

(define (fmt-date date)
  (let ((fmt (cond ((= (date-year date) (date-year (current-date)))
                    "~b ~e ~H:~M")
                   (else
                    "~b ~e  ~Y"))))
    (date->string date fmt)))

(define (get-stats pathname)
  (let ((exists? (file-exists? pathname)))
    (list (and exists? (file-size-in-bytes pathname))
          (and exists? (fmt-date (time-utc->date (file-modification-time pathname))))
          (file-namestring pathname))))

(main (command-line))
