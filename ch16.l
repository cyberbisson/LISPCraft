;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LISPCraft (by Robert Wilensky)
;; Excercies from Chapter 16 (Error Handling and Nonstardard Flow of Control)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matt Bisson (June 2013)

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
;; (2) Use "catch" and "throw" to implement a perfect number generator.  Recall
;;     that perfect numbers are equal to the sum of their proper divisors.
;;     Write this code by writing a low level function that searches for the
;;     next perfect number each time it is called.  When it finds one, it throws
;;     it to the calling routine.  This calling routine should consist of a loop
;;     that catches that value thrown by the low level routine, prints it out,
;;     and iterates this procedure.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-condition perfect-number-found (error)
  ((value :intarg)))

(defun compute-factors (number)
  "Return a list of the factors for a number (not including the number itself)."

  (cond
   ((or (>= 1 number) (not (integerp number))) nil)
   (t
    (do
     ((check-num (- number 1) (- check-num 1))
      (results   '()
                 (if (= 0 (mod number check-num))
                     (cons check-num results)
                   results)))
     ((= check-num 0) results)))))

(defun compute-perfect-number (target)
  "Compute if a number is a perfect number.  If so, return the number.  Otherwise, return nil."

  (let ((factors (compute-factors target)))
    (do ((result       (car factors) (+ result (car rest-factors)))
         (rest-factors (cdr factors) (cdr rest-factors)))
        ((null rest-factors) nil)

        (when (or (null result) (= target result))
           (error 'perfect-number-found result)))))

(defun compute-perfect-numbers (start end)
  "Compute the set of perfect numbers that exist between the range [start,end]."

  (let ((result '()))
    (do ((i start (+ i 1)))
        ((= i end) (nreverse result))
        (handler-case
         (compute-perfect-number i)
         (perfect-number-found (val) (setq result (cons val result))))))
)

(defun sanity-test-16-2 ()

  (sanity-check-result (compute-perfect-numbers 0 1) '())
  (sanity-check-result (compute-perfect-numbers 1 6) '(6))
  (sanity-check-result (compute-perfect-numbers 1 30) '(6 28))
  (sanity-check-result (compute-perfect-numbers 6 28) '(6 28))
  (sanity-check-result (compute-perfect-numbers 1 1000) '(6 28 496))
  (sanity-check-result (compute-perfect-numbers 1 10000) '(6 28 496 8128))
; (sanity-check-result
;  (compute-perfect-numbers 1 1000000000)
;  '(6 28 496 8128 33550336))
)
