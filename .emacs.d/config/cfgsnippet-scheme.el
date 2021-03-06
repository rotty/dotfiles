;;(eval-after-load "scheme" '(modify-syntax-entry ?, "'   " scheme-mode-syntax-table))

(require 'cl)

;;; Derick's scheme.el
(require 'scheme)

;; bracketphobia, http://www.emacswiki.org/cgi-bin/wiki/bracketphobia.el
(require 'bracketphobia)

(autoload 'inferior-scheme "inferior-scheme" "Run an inferior Scheme process." t)
(setq inferior-scheme-command-line "ikarus -d")

(setq ikarus-program-arguments '("--r6rs-script"))

(dolist (elt '(("\\.sls$" . scheme-mode)
	       ("\\.sps$" . scheme-mode)))
  (add-to-list 'auto-mode-alist elt))

;; paredit.el
(require 'paredit)

;; override these paredit bindings, they annoy the hell out of me
;; (they are bound to other key sequences as well, so no
;; functionality loss)
(define-key paredit-mode-map (kbd "C-<left>") nil)
(define-key paredit-mode-map (kbd "C-<right>") nil)

(defun my-lispy-mode-hook ()
  (enable-paredit-mode)
  (set (make-local-variable 'comment-add) 1)
  (local-set-key (kbd "M-/") (make-hippie-expand-function
			      '(try-my-dabbrev-substring
				try-expand-dabbrev-visible
				try-expand-dabbrev-from-kill
				try-expand-dabbrev-all-buffers
				try-complete-file-name-partially
				try-complete-file-name))))

(dolist (hook '(emacs-lisp-mode-hook lisp-mode-hook scheme-mode-hook))
  (add-hook hook 'my-lispy-mode-hook))

;; Disable quack (from emacs-goodies-el)
(remove-hook 'scheme-mode-hook 'quack-scheme-mode-hookfunc)


;; ikarus.el
(eval-after-load 'scheme
  '(progn
     (define-key scheme-mode-map (kbd "C-c i") 'ikarus-run)
     (define-key scheme-mode-map (kbd "C-c r") 'ikarus-rerun)))


;; scheme-complete
;;(autoload 'scheme-smart-complete "scheme-complete" nil t)
;;(autoload 'scheme-complete-or-indent "scheme-complete" nil t)
;;(autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)

;; (eval-after-load 'scheme
;;   '(progn (define-key scheme-mode-map "\t" 'scheme-complete-or-indent)))

;; (add-hook 'scheme-mode-hook
;;    (lambda ()
;;      (make-local-variable 'eldoc-documentation-function)
;;      (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
;;      (eldoc-mode)))



(put 'scheme48-package 'safe-local-variable 
     #'(lambda (v)
         (or (symbolp v)
             (and (listp v)
                  (every #'symbolp v)))))



(scheme-set-indent-form-names 
 '((scheme-indent-defform "define-.*")
   (scheme-indent-withform "with-.*")
   (1 "parameterize")
   (2 "trace-lambda")
   (scheme-let-indent "and-let\\*\\|let-optionals\\*?")
   (scheme-indent-receive "receive")))

(scheme-add-indent-style 
 'testeez
 '((testeez 1)
   (test/equal 1)
   (test/equiv 1)
   (test-true 1)
   (test-eval 1)
   (test-define 2)))

(scheme-add-indent-style
 'trc-testing
 '((test-eq 1)
   (test-equal 1)
   (test-eqv 1)
   (test-compare 2)))

(scheme-add-indent-style
 'sbank
 '((let-attributes 3)
   (let-accessors 2)
   (send 1)))

(scheme-add-indent-style
 'conjure-dsl
 '((project 1)
   (define-project 2)))

(scheme-add-indent-style
 'foof-loop
 '((loop scheme-let-indent)
   (iterate with-...)
   (iterate! with-...)
   (iterate* with-...)
   (iterate-values with-...)
   (lazy-recur with-...)
   (lazy-recur* with-...)
   (recur with-...)
   (recur* with-...)

   ;;; This is silly, but so would altering the definition of
   ;;; `scheme-indent-function' yet again to include a test for
   ;;; `collect-...'.  Better would be to have a table mapping regular
   ;;; expressions to indent functions, as Edwin has.  But this is
   ;;; expedient for now.

   (collect-average with-...)
   (collect-display with-...)
   (collect-list with-...)
   (collect-list! with-...)
   (collect-list-into! with-...)
   (collect-list-reverse with-...)
   (collect-max with-...)
   (collect-min with-...)
   (collect-product with-...)
   (collect-stream with-...)
   (collect-string with-...)
   (collect-string-of-length with-...)
   (collect-sum with-...)
   (collect-vector with-...)
   (collect-vector-of-length with-...)

   ;;; This one doesn't follow the same pattern as the others, because
   ;;; there is no expression; (COLLECT-COUNT ...) is the same as
   ;;; (COLLECT-SUM ... 1).

   (collect-count 0)
   ))

(scheme-add-indent-style
 'parscheme
 '((parser:map 1)
   ("^parser:\\(optional\\|repeated\\)$" 1)
   ("^parser:.*:repeated-until$" 1)
   (*parser scheme-indent-withform)))

(dolist (hint
	 '((with-test-prefix 1)
	   (with-interaction-environment 1)
	   (with-benchmark-prefix  1)
	   (letrec 1)
	   (make-method 1)
	   (add-method! defun)
	   (let-slots 2)
	   (let-optionals 2)
	   (let-optionals* 2)
	   (eval-when 1)
	   (let-fluids 4)
	   (destructure 1)
	   (scmxlate-macro 1)
	   (make-ctype 1)
	   (match 1)
	   (let-keywords 3)
           (letrec* 1)
	   (dir-excursion 1)
	   (tla-dir-excursion 2)
	   (remote-apply 1)
	   (remote-run! 1)
	   (open-tcp-listener-accept-loop 1)
	   (restart-command-processor 2)
	   (let-fluid 2)
	   (define-peephole-optimizer 2)
           (in-test-group 1)
	   (parse 1)
	   (object 1)
	   (set-standard-read-macro! 2)
	   (add-tests-with-string-output 1)
	   (guard 1)
	   (library 1)
	   (trace-define 1)
	   (trace-lambda 2)
	   (let-callouts 2)
	   (and-let* 1)
	   (send 1)
	   (flags-case 1)
	   (coroutine 1)
	   (repeat 1)
	   (let-privates 1)
           (let*-props 2)
           (program 1)
           (fmt-let 2)))
  (put (car hint) 'scheme-indent-function (cadr hint)))

(require 'ikarus)


;;; This could be generalized to negative special form indent methods; e.g.,
;;;
;;;   (put 'with-frobbotzim 'scheme-indent-function -2)
;;;
;;; and then
;;;
;;;   (with-frobbotzim frob grovel
;;;       full lexical
;;;       mumble chumble
;;;       spuzz
;;;     (lambda (foo) ...)
;;;     (lambda (bar) ...))
;;;
;;; That is, the last two subforms would be indented two spaces, whereas all
;;; preceding subforms would get four spaces.

(defun scheme-indent-withform (state indent-point normal-indent)
  (if (not (and (boundp 'paredit-mode)
                paredit-mode))
      ;; If we're not in paredit mode, it's not really safe to go backwards
      ;; from the end and to try to indent based on that, since there may not
      ;; be an end to work backwards from (i.e. the structure not be valid).
      (scheme-indent-defform state indent-point normal-indent)
    (goto-char (nth 1 state))
    (let ((body-column (+ (current-column)
                          lisp-body-indent)))
      (forward-sexp 1)
      (backward-char 1)
      (backward-sexp 1)
      (skip-chars-backward " \t" (point-at-bol))
      (if (= (point) indent-point)
          body-column
          ;; If it's not the last argument, then we must specify not only the
          ;; column to indent to but also the start of the containing sexp,
          ;; which implies (don't ask me how) that any *following* subforms
          ;; must be indented separately, and not just on this column.  This
          ;; allows C-M-q to know to indent the penultimate arguments with four
          ;; spaces, but to keep recomputing the indentation so that it doesn't
          ;; assume the last one will go to the same column, which is a wrong
          ;; assumption.
          (list (+ body-column lisp-body-indent)
                (nth 1 state))))))

(put 'let-fluids 'scheme-indent-function 'with-...)

;;; Nested foof-loop forms


;;;; RECEIVE Indentation

;;; (RECEIVE <bvl>           ; Line up BVL & producer.
;;;          <producer>
;;;   <body>)

(defun scheme-indent-receive (state indent-point normal-indent)
  (let ((containing-form-start (nth 1 state))
        (i 0)
        containing-form-column)
    ;; <snip documentation>
    (goto-char containing-form-start)
    (setq containing-form-column (current-column))
    (forward-char 1)
    (forward-sexp 1)
    ;; Now find the start of the last form.
    (parse-partial-sexp (point) indent-point 1 t)
    (while (and (< (point) indent-point)
                (condition-case ()
                    (progn
                      (setq i (1+ i))
                      (forward-sexp 1)
                      (parse-partial-sexp (point) indent-point 1 t))
                  (error nil))))
    ;; Point is sitting on first character of last (or count) sexp.
    (cond ((= i 0)
           (+ containing-form-column (* 2 lisp-body-indent)))
          ((= i 1) (list normal-indent containing-form-start))
          (t (+ containing-form-column lisp-body-indent)))))

(put 'receive 'scheme-indent-function 'scheme-indent-receive)

