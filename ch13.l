;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LISPCraft (by Robert Wilensky)
;; Excercies from Chapter 13 (Macros)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matt Bisson (May 2013)

(defmacro sanity-check-result (result expected-result)
  "Check the result of exercies, and validate that they match expectations."

  `(let ((tmp-res ,result) ; TODO: Generate these names...
         (tmp-exp ,expected-result))
     (if (equal tmp-res tmp-exp)
         (format t "Test passed: result = ~D~%"         tmp-exp)
         (format t "Test FAILED: \"~S\" != \"~S\" - Expected ~D, Got ~D~%"
                 '(,@expected-result) '(,@result)
                 tmp-exp tmp-res))

  ;; Just return the value in case...
  (equal tmp-res tmp-exp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (1) Write macros "head" and "tail" which expand into calls to "car" and
;;     "cdr," respectively.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro head (l)
  "Redefind CAR as a macro"
  `(car ,l))

(defmacro tail (l)
  "Redefine CDR as a macro"
  `(cdr ,l))

(defun sanity-test-13-1 ()

  ; Test HEAD vs CAR
  (sanity-check-result (head '())          (car '()))
  (sanity-check-result (head '(a . b))     (car '(a . b)))
  (sanity-check-result (head '(a b c))     (car '(a b c)))
  (sanity-check-result (head '((a b) c))   (car '((a b) c)))
  (sanity-check-result (head '(a (b c) d)) (car '(a (b c) d)))
  (sanity-check-result (head '(a ((((b ((((c))))))))))
                       (car  '(a ((((b ((((c)))))))))))

  ; Test TAIL vs CDR
  (sanity-check-result (tail '())          (cdr '()))
  (sanity-check-result (tail '(a . b))     (cdr '(a . b)))
  (sanity-check-result (tail '(a b c))     (cdr '(a b c)))
  (sanity-check-result (tail '((a b) c))   (cdr '((a b) c)))
  (sanity-check-result (tail '(a (b c) d)) (cdr '(a (b c) d)))
  (sanity-check-result (tail '(a ((((b ((((c))))))))))
                       (cdr  '(a ((((b ((((c)))))))))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (2) Write a macro "IF" that translates calls of the form (IF a THEN b) into
;;     conds of the form (cond (a b)), and calls of the form
;;     (IF a THEN b ELSE c) into conds of the form (cond (a b) (t c)).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro my-if (a then b &rest else-c)
  "A simple IF THEN ELSE construct, where the ELSE clause is optional."

  `(cond
    ((not (eq ',then 'then)) nil ; TODO -- thow an error!
    (,a ,b)
    (t (eval (cadr '(,@else-c))))))

(defun sanity-test-13-2 ()

  ;; If, then...
  (sanity-check-result (my-if nil then 'a)
                       (if    nil      'a))
  (sanity-check-result (my-if t then (cons 'a 'b))
                       (if    t      (cons 'a 'b)))

  (sanity-check-result (my-if nil then (cons 'a 'b))
                       (if    nil      (cons 'a 'b)))
  (sanity-check-result (my-if t then (cons 'a 'b))
                       (if    t      (cons 'a 'b)))

  (sanity-check-result (my-if (= 0 1) then (cons 'a 'b))
                       (if (= 0 1) (cons 'a 'b)))
  (sanity-check-result (my-if (= 1 1) then (cons 'a 'b))
                       (if (= 1 1) (cons 'a 'b)))

  ;; If, then, else...
  (sanity-check-result (my-if nil then 'a else 'b)
                       (if    nil      'a      'b))
  (sanity-check-result (my-if t then (cons 'a 'b) else (list 'a 'b))
                       (if    t      (cons 'a 'b)      (list 'a 'b)))

  (sanity-check-result (my-if nil then (cons 'a 'b) else (list 'a 'b))
                       (if    nil      (cons 'a 'b)      (list 'a 'b)))
  (sanity-check-result (my-if t then (cons 'a 'b) else (list 'a 'b))
                       (if    t      (cons 'a 'b)      (list 'a 'b)))

  (sanity-check-result (my-if (= 0 1) then (cons 'a 'b) else (list 'a 'b))
                       (if    (= 0 1)      (cons 'a 'b)      (list 'a 'b)))
  (sanity-check-result (my-if (= 1 1) then (cons 'a 'b) else (list 'a 'b))
                       (if    (= 1 1)      (cons 'a 'b)      (list 'a 'b)))

  ;; TODO - negative tests and things that cause an error if both cases are
  ;;        evaluated.
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (3) Write a version of the "for" macro described above that provides a
;;     uniform way of expressing calls to mapping functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 1. (for (x in l) (do (foo x)))   => (mapc 'foo l)
; 2. (for (x on l) (do (foo x)))   => (map 'foo l)
; 3. (for (x in l) (save (foo x))) => (mapcar 'foo l)
; Optional 4. (for (x in l) (when (test x)) (do (foo x))) =>
;             (mapc '(lambda (x) (cond ((test x) (foo x)))) l)
(defmacro my-for (clause-1 clause-2 &optional clause-3)
  "A fairly complex for-loop construct..."

                                        ; Maybe rewrite this by putting verbs in
                                        ; a single list?
  (let ((verb-1 (cadr clause-1))
        (verb-2 (car  clause-2))
        (verb-3 (car  clause-3)))

    ;; Todo check for DO -- we're only checking SAVE
    (let ((save-fn
           (if (eq verb-2 'when) (eq verb-3 'save) (eq verb-2 'save))))
      (list
       ; Call the method...
       (cond
        ((eq 'in verb-1) (if save-fn 'mapcar 'mapc))
        ((eq 'on verb-1) (if save-fn 'maplist 'map))
        (t nil)) ; TODO ERROR!!!

       ; Build the action...
       (let ((varname (car clause-1)))
         (list 'lambda (list varname)
               (if (eq verb-2 'when)
                   `(cond (,(cadr clause-2) ,(cadr clause-3)))
                 (cadr clause-2))))

       ; Call the lambda...
       (caddr clause-1)))))

(defun sanity-test-13-3 ()
  (let ((test-list '(1 2 3 4 5)))

    ;; Testing CAR functionality
    (sanity-check-result (my-for (x in test-list) (do (+ x 1)))
                         (mapc (lambda (x) (+ x 1)) test-list))
    (sanity-check-result (my-for (x in test-list) (save (+ x 1)))
                         (mapcar (lambda (x) (+ x 1)) test-list))
    (sanity-check-result (my-for (x in test-list) (when (= x 5)) (save (+ x 1)))
                         (mapcar (lambda (x) (if (= x 5) (+ x 1))) test-list))

    ;; Testing LIST functionality
;   (sanity-check-result (my-for (x on test-list) (do (listp x)))
;                        (map (lambda (x) (listp x)) test-list))
;   (sanity-check-result (my-for (x on test-list) (save (listp x)))
;                        (maplist (lambda (x) (listp x)) test-list))
;   (sanity-check-result (my-for (x on test-list)
;                                (when (evenp (car x)))
;                                (save (listp x)))
;                        (maplist (lambda (x) (if (evenp (car x)) (listp x)))
;                                 test-list))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (4) Some LISP programmers prefer not to use "go"s explicitly in their code.
;;     Instead, they write macros that expand into "prog"s.
;;
;;     Define a macro "while" that can be called with expressions of the form
;;     (while test exp1 exp2 ...).  As long as the expression "test" evaluates
;;     to true, "while" repeatedly evaluates the expressions "exp1", "exp2", etc.
;;
;;     while should translate into a call to "prog" which contains an explicit
;;     call to "go".  It is possible to write a more complete (and more
;;     complicated) version iteration macro.  See Charniak et al. (1979) for an
;;     example of such a macro.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro my-while (test &rest exprs)
  "Write a nifty macro to do a while loop"

  `(prog ()
     loop
     (unless ,test (return nil)) ; TODO - what am I returning??
     ,@exprs
     (go loop))
)

(defun sanity-test-13-4 ()
  (let ((i       0)
        (results '()))
    (my-while (< i 5)
              (setq results (cons i results))
              (setq i (+ i 1)))
    results)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (5) Write the functions "de" and "df" as macros.  Recall that "de" and "df"
;;     are used to define exprs and fexprs, respectively.  Write a macro "dm"
;;     that can be used similarly to define macros.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
