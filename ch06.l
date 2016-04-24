;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LISPCraft (by Robert Wilensky)
;; Excercies from Chapter 6 (Iteration)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matt Bisson (May 2013)

(defun sanity-check-result (result expected-result)
  "Check the result of exercies, and validate that they match expectations."

  (if (equal result expected-result)
      (format t "Test passed: result = ~D~%"         expected-result)
      (format t "Test FAILED: Expected ~D, Got ~D~%" expected-result result))

  ;; Just return the value in case...
  (equal result expected-result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (1) Write iterative versions of the functions "factorial," "power-of-two,"
;;     "remove," and "assoc," which were defined in the last chapter.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun factorial (input)
  "Iteratively compute facrtorial"

  (if (> 0 input)
      nil
    (do ((n      (floor input) (- n 1))
         (result 1             (* result n)))
        ((> 1 n) result))))

;; Power of two:

(defun iterative-power (base pow)
  "Assuming a power of 0 or greater, x^y."

  (do ((n pow (- n 1))
       (result 1 (* result base)))
      ((= 0 n) result)))

(defun iterative-inverse-power (base pow)
  "Assuming a power of 0 or less, x^y."

  (do ((n pow (+ n 1))
       (result 1 (/ result base)))
      ((= 0 n) result)))

(defun power-of-two (input)
  "Iteratively return a power of two based on the given input (as exponent)."

  (if (< 0 input) (iterative-power 2 input)
    (iterative-inverse-power 2 input)))

;; Remove:

(defun my-remove (to-remove input)
  "Highly inefficient iterative element removal."

  (do ((to-process (reverse input) (cdr to-process)) ;; Note the reversal!
       (result     '()
                   (if (equal to-remove (car to-process))
                       result
                       (cons (car to-process) result))))
      ((null to-process) result)))

;; Assoc:

(defun my-assoc (key input)
  (do ((rest input (cdr rest)))
      ((or (eq key (caar rest))
           (null (car rest)))
       (car rest))))

;; Tests:

(defun sanity-test-6-1 ()
  "Test that the methods return the expected values without crashing."

  ;; Factorial tests...
  (sanity-check-result (factorial -1) nil)
  (sanity-check-result (factorial 0) 1)
  (sanity-check-result (factorial 1) 1)
  (sanity-check-result (factorial 2) 2)
  (sanity-check-result (factorial 3) 6)
  (sanity-check-result (factorial 4) 24)
  (sanity-check-result (factorial 5) 120)
  (sanity-check-result (factorial 6) 720)
  (sanity-check-result (factorial 7) 5040)
  (sanity-check-result (factorial 8) 40320)
  (sanity-check-result (factorial 5.1) 120)
  (sanity-check-result (factorial 11/2) 120)

  ;; Basic power tests...
  (sanity-check-result (power-of-two 0) 1)
  (sanity-check-result (power-of-two 1) 2)
  (sanity-check-result (power-of-two 2) 4)
  (sanity-check-result (power-of-two 3) 8)
  (sanity-check-result (power-of-two 4) 16)
  (sanity-check-result (power-of-two 5) 32)
  (sanity-check-result (power-of-two 6) 64)
  (sanity-check-result (power-of-two 7) 128)
  (sanity-check-result (power-of-two 8) 256)

  ;; Larger powers...
  (sanity-check-result (power-of-two 16) 65536)
  (sanity-check-result (power-of-two 32) 4294967296)
  (sanity-check-result (power-of-two 64) 18446744073709551616)
  (sanity-check-result
   (power-of-two 128)
   340282366920938463463374607431768211456)

  ;; Inverse (power) tests...
  (sanity-check-result (power-of-two -1) 1/2)
  (sanity-check-result (power-of-two -2) 1/4)
  (sanity-check-result (power-of-two -3) 1/8)
  (sanity-check-result (power-of-two -4) 1/16)
  (sanity-check-result (power-of-two -5) 1/32)
  (sanity-check-result (power-of-two -6) 1/64)
  (sanity-check-result (power-of-two -7) 1/128)
  (sanity-check-result (power-of-two -8) 1/256)

  ;; Remove tests...
  (sanity-check-result (my-remove 'b     '(a b c b d))         '(a c d))
  (sanity-check-result (my-remove '(a b) '(a (a b) c (a b) d)) '(a c d))

  ;; Assoc tests...
  (sanity-check-result
   (my-assoc 'y '((x a) (y b) (z c)))
   (assoc    'y '((x a) (y b) (z c))))

  (sanity-check-result
   (my-assoc 'a '((x a) (y b) (z c)))
   (assoc    'a '((x a) (y b) (z c))))

  (sanity-check-result
   (my-assoc 'l '((x a) (y b) (z c)))
   (assoc    'l '((x a) (y b) (z c))))

  (sanity-check-result
   (my-assoc '(y b) '((x a) ((y b) b) (z c)))
   (assoc    '(y b) '((x a) ((y b) b) (z c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (2) Write an iterative version of the function "make-assoc-list," which makes
;;     a list of the sort "assoc" uses out of two lists of items to be paired
;;     with one another.  For example, (make-assoc-list '(a b c d) '(1 2 3 4))
;;     returns ((a 1) (b 2) (c 3) (d 4)).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-make-assoc-list (keys values)
  "Iteratively create a list of key/value lists."

  (do* ; Note: serialized!
   ((keys-rest   keys   (cdr keys-rest))
    (values-rest values (cdr values-rest))
    (result
     (list (list (car keys)      (car values)))
     (cons (list (car keys-rest) (car values-rest)) result)))
   ((or (null (cdr keys-rest))
        (null (cdr values-rest)))
    (reverse result)))) ; Note: reversed the result!

(defun sanity-test-6-2 ()
  (sanity-check-result (my-make-assoc-list '(a b c d) '(1 2 3 4))
                       '((a 1) (b 2) (c 3) (d 4)))

  (sanity-check-result (my-make-assoc-list '(a b c) '(1 2 3 4))
                       '((a 1) (b 2) (c 3)))

  (sanity-check-result (my-make-assoc-list '(a b c d) '(1 2 3))
                       '((a 1) (b 2) (c 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (3) Write a function "sub-splice" which is like "subst," but which "splices
;;     in" its first argument for the second.  For example,
;;     (sub-splice '(1 2) 'b '(a b c)) returns (a 1 2 c);
;;     (sub-splice '(1 2) 'b '(a (b c) d)) returns (a (1 2 c) d).  Decide which
;;     is simpler, a recursive or iterative version of this function.  Implement
;;     the simpler version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; I'm going with the recursive version, because iterating seems to require that
; I (at least) reverse the list before I return it.  Also, recursion is easy, as
; you basically do a depth-first traversal, and either return the cons-ed list
; with (car input) or "replacement-obj" as the first parameter...
(defun sub-splice (replacement-obj to-replace input)
  (cond
   ((null input) nil)
   ((listp (car input))
    (cons
     (sub-splice replacement-obj to-replace (car input))
     (sub-splice replacement-obj to-replace (cdr input))))
   (t
    (if (eq to-replace (car input))
        (append
         replacement-obj
         (sub-splice replacement-obj to-replace (cdr input)))
        (cons
         (car input)
         (sub-splice replacement-obj to-replace (cdr input)))))))

(defun sanity-test-6-3 ()
  (sanity-check-result (sub-splice '(1 2) 'b '(a b c)) '(a 1 2 c))
  (sanity-check-result (sub-splice '(1 2) 'b '(a (b c) d)) '(a (1 2 c) d)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (4) Write a LISP function that computes "perfect numbers."  A perfect number
;;     is defined as a number whose proper divisors sum to that number.  For
;;     example, 6 is a perfect number because the sum of the proper divisors of
;;     6, namely, 1, 2, and 3, is equal to 6; 28 is a perfect number because the
;;     proper divisors of 28 are 1, 2, 4, 7, and 14, which add up to 28.  You
;;     may write this as a function that finds all perfect numbers between a
;;     range of two number supplied as arguments.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
        ((null rest-factors)
         (if (or (null result) (= target result)) result nil)))))

(defun compute-perfect-numbers (start end)
  "Compute the set of perfect numbers that exist between the range [start,end]."

                                        ; start-1 because computation begins
                                        ; after the first iteration/check
  (do* ((target (- start 1) (+ target 1))
        (perfect-numbers
         '()
         (let ((result (compute-perfect-number target)))
           (if (null result)
               perfect-numbers
               (cons result perfect-numbers)))))
       ((= target end) (reverse perfect-numbers))))

(defun sanity-test-6-4 ()

  (sanity-check-result (compute-perfect-number -1) nil)
  (sanity-check-result (compute-perfect-number 0) nil)
  (sanity-check-result (compute-perfect-number 1) nil)
  (sanity-check-result (compute-perfect-number 2) nil)
  (sanity-check-result (compute-perfect-number 3) nil)
  (sanity-check-result (compute-perfect-number 4) nil)
  (sanity-check-result (compute-perfect-number 5) nil)
  (sanity-check-result (compute-perfect-number 6) 6)
  (sanity-check-result (compute-perfect-number 7) nil)
  (sanity-check-result (compute-perfect-number 8) nil)
  (sanity-check-result (compute-perfect-number 9) nil)
  (sanity-check-result (compute-perfect-number 10) nil)
  (sanity-check-result (compute-perfect-number 28) 28)
  (sanity-check-result (compute-perfect-number 496) 496)
  (sanity-check-result (compute-perfect-number 8128) 8128)
  (sanity-check-result (compute-perfect-number 33550336) 33550336)
; (sanity-check-result (compute-perfect-number 8589869056) 8589869056)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (5) Suppose we represented a matrix in LISP as a list of lists.  For example,
;;     ((a b) (c d)) would represent a 2x2 matrix whose first row contains the
;;     elements a and b, and whose second row has the elements c and d.  Write a
;;     function that takes a matrix as input, and outputs its transpose.  For
;;     example, (transpose '((a b) (c d))) should return ((a c) (b d)).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun transpose (matrix)
  nil
)

(defun sanity-test-6-5 ()
  (sanity-check-result
   (transpose
    '((a b)
      (c d)))
   '((a c)
     (b d)))

  (sanity-check-result
   (transpose 
    '((1 2)
      (3 4)
      (5 6)))
   '((1 3 5)
     (2 4 6)))

  (sanity-check-result
   (transpose 
    '((1 3 5)
      (2 4 6)))
   '((1 2)
     (3 4)
     (5 6))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (6) Write a function "intersect" that computes the set intersection of two
;;     lists.  The set intersection of two lists is the list of elements that
;;     occur in both lists.  For example, (intersect '(a b b c d) '(c a b b))
;;     returns (a b c).  Note that we ignore the order of elements in a list as
;;     far as set intersection is concerned.  In addition, an element should
;;     appear only once in the result, even if it appears repeatedly in the
;;     arguments.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun intersect-recurse (list-1 list-2 result)
  (cond
   ((null list-1) (reverse result))
   (t
    (let ((elem (car list-1)))
      (if (or (null (member elem list-2))
              (not (null (member elem result))))
          (intersect-recurse (cdr list-1) list-2 result)
          (intersect-recurse (cdr list-1) list-2 (cons elem result)))))))

(defun intersect (list-1 list-2)
  (intersect-recurse list-1 list-2 '()))

(defun sanity-test-6-6 ()
  (sanity-check-result (intersect '(a b b c d) '(c a b b)) '(a b c))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (7) Do all recursive functions have an iterative, non-recursive version?
;;     Consider the functions "subst" and "count-atoms" described in the last
;;     chapter.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; It is difficult, but not impossible to write an iterative function for list
; (or tree) structures that are arbitrarily nested (or deep).  One would have to
; execute conditional logic if they encountered a leaf node vs. a subtree, and
; keep a trail back to the parent (if none existed in the datatype itself).
