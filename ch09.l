;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LISPCraft (by Robert Wilensky)
;; Excercies from Chapter 9 (Lambda)
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
;; (1) Evaluate the following s-expressions:
;;    (a) ((lambda (x) (cons x nil)) 'y)
;;    (b) (apply '(lambda (x y) (list y x)) '(1 2))
;;    (c) (mapcar '(lambda (x) (quotient 1 (sqrt x))) '(1 2 3 4 5))
;;    (d) ((lambda (x) (setq x 5)) (setq x 4))
;;    (e) ((lambda (x y) (putprop x y 'eats)) 'bird 'worms)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (a) (Y)
; (b) (2 1)
; (c) (1.0 0.70710677 0.57735026 0.5 0.4472136)
; (d) 5 ; (x = 4)
; (e) WORMS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (2) A common use of lambda is to create a new function out of an existing
;;     function by "fixing" one of the existing function's arguments.  Use
;;     lambda to create a function that uses "time" to alwyas multiply its
;;     argument by 3.14.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; I don't understand the question...
(defun times-pi (x)
  ((lambda (x1) (* 3.14 x1)) x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (3) The function "sum-loop" in Chapter 8 takes a function and a number, and
;;     returns the sum of that function applied to the integers up to that
;;     number.  Suppose we want to use "sum-loop" just to compute the sum of the
;;     first n integers.  What function do we have to pass to "sum-loop" to do
;;     this computation?  Write this function using lambda and call "sum-loop"
;;     with it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sum-loop (func n)
  "...Stolen from Chapter 8, and modified to actually work."

  (prog (result)
        (setq result 0)
        loop
        (cond ((zerop n) (return result)))
        (setq result (+ (funcall func n) result))
        (setq n (- n 1))
        (go loop)))

(defun sanity-test-9-3 ()
  (sanity-check-result (sum-loop (function (lambda (x) x)) 5) (+ 5 4 3 2 1))
)
