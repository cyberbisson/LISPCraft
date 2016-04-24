;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LISPCraft (by Robert Wilensky)
;; Excercies from Chapter 5 (Recursion)
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
;; (1) Write a recursive function "power-of-two" that computes the nth power of
;;     2.  E.g., (power-of-two 8) returns 256.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun power-of-two (pow)
  "Computes 2 raised to the power specified in the argument."
  (optimized-power 2 pow))

(defun simple-power (base pow)
  "This implementation of x^y is not tail-recursive..."
  (cond
   ((eq 0 pow) 1)
   ((eq 1 pow) base)
   ((> 0 pow)
    (/ (simple-power base (+ pow 1)) base))
   (t
    (* (simple-power base (- pow 1)) base))))

(defun tail-recursive-power (base pow result)
  "This method should not be called directly.  It is a tail recursive call for
 x^y where y is positive (or 0)."

  (cond
   ((eq 0 pow) result)
   ((eq 1 pow) (* base result))
   (t (tail-recursive-power base (- pow 1) (* base result)))))

(defun tail-recursive-power-inverse (base pow result)
  "This method should not be called directly.  It is a tail recursive call for
 x^y where y is negative."

  (cond
   ((eq 0 pow) result)
   ((eq -1 pow) (/ result base))
   (t (tail-recursive-power-inverse base (+ pow 1) (/ result 2)))))

(defun optimized-power (base pow)
  "This implementation of x^y uses tail recursion to optimize performance."
  (if (> 0 pow)
      (tail-recursive-power-inverse base pow 1)
      (tail-recursive-power         base pow 1)))

;; Tests...
(defun sanity-test-5-1 ()
  "Test that the methods return the expected values without crashing."

  (sanity-check-result (power-of-two 0) 1)
  (sanity-check-result (power-of-two 1) 2)
  (sanity-check-result (power-of-two 2) 4)
  (sanity-check-result (power-of-two 3) 8)
  (sanity-check-result (power-of-two 4) 16)
  (sanity-check-result (power-of-two 5) 32)
  (sanity-check-result (power-of-two 6) 64)
  (sanity-check-result (power-of-two 7) 128)
  (sanity-check-result (power-of-two 8) 256)

  (sanity-check-result (power-of-two 16) 65536)
  (sanity-check-result (power-of-two 32) 4294967296)
  (sanity-check-result (power-of-two 64) 18446744073709551616)
  (sanity-check-result
   (power-of-two 128)
   340282366920938463463374607431768211456)

  (sanity-check-result (power-of-two -1) 1/2)
  (sanity-check-result (power-of-two -2) 1/4)
  (sanity-check-result (power-of-two -3) 1/8)
  (sanity-check-result (power-of-two -4) 1/16)
  (sanity-check-result (power-of-two -5) 1/32)
  (sanity-check-result (power-of-two -6) 1/64)
  (sanity-check-result (power-of-two -7) 1/128)
  (sanity-check-result (power-of-two -8) 1/256))

(defun stress-test-5-1 ()
  "Compare operation of the tail-call optimized power method and the simple one."

  (format t
          "Optimized power test completed: ~D~%~%"
          (optimized-power 2 100000))

  ; Because it's not tail-recursive, this test doesn't even complete!
  (format t
          "Simple power test completed: ~D~%~%"
          (simple-power    2 100000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (2) Write a recursive function "count-atoms" that counts the number of atoms
;;     that appear at all levels of an s-expression.  For example,
;;     (count-atoms '(a (b c) (d (e) f))) returns 6.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun count-atoms (input)
  "Count the number of atoms in the list.  Tracking nil is difficult (doesn't
 work)..."

  (cond
   ((eq nil input) 0)
   ((atom input)   1)
   ((listp input)
    (+ (count-atoms (car input)) (count-atoms (cdr input))))))

(defun sanity-test-5-2 ()
  "Check a bunch of different lists, and see if the result makes sense..."

  (sanity-check-result (count-atoms nil) 0)
  (sanity-check-result (count-atoms 'a) 1)
  (sanity-check-result (count-atoms '(a (b c) (d (e) f))) 6)
  (sanity-check-result (count-atoms
                        (list 'a (list 'b 'c) (list 'd (list 'e) 'f nil)))
                        7)

  (sanity-check-result (count-atoms
                        (list 'a (list 'b 'c) (list 'd (list 'e "text") 'f nil)))
                        8)

  (sanity-check-result (count-atoms
                        (list 1 (list 'b 'c) (list 'd (list 'e "text") 'f nil)))
                        8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (3) Write a recursive function "remove" that removes all the occurrences of
;;     an element from a list.  For example, (remove 'b '(a b c b d)) returns
;;     (a c d).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-remove (to-remove input)
  "A simple (non-tail-optimized) element removal function."

  (let ((popped (car input)))
    (cond
     ((equal popped to-remove) (my-remove to-remove (cdr input)))
     ((null popped)         nil)
     (t
      (cons (car input) (my-remove to-remove (cdr input)))))))

(defun sanity-test-5-3 ()
  (sanity-check-result (my-remove 'b     '(a b c b d))         '(a c d))
  (sanity-check-result (my-remove '(a b) '(a (a b) c (a b) d)) '(a c d)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (4) Use recursion to write your own version of the LISP function "assoc."
;;     assoc "looks up" a value in a list of lists, and returns the first list
;;     it finds whose first element matches the desired value.  For example,
;;     (assoc 'y '((x a) (y b) (z c))) returns (y b).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-assoc (key input)
  "Custom implementation of 'assoc' (with tail recursion)."

  (cond
   ((null (car input))    nil)
   ((eq (caar input) key) (car input))
   (t                     (my-assoc key (cdr input)))))

(defun sanity-test-5-4 ()
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
;; (5) A common mistake involving recursion takes the following form.  Suppose
;;     we want to write a function "if" that uses an "if...then...else" format.
;;     We might try to do this as follows:
;;       -> (defun if (condition then-action else-action)
;;       ->  (cond (condition then-action) (t else-action)))
;;       if
;;       -> (if (< 4 5) 'yes 'no)
;;       yes
;;       -> (if (> 4 5) 'yes 'no)
;;       no
;;     "if" seems to work.  But what might happen if we use "if" inside a
;;     recursive definition?  For example, suppose we wrote "factorial" in terms
;;     of "if":
;;       -> (defun factorial (n)
;;       ->   (if (zerop n) 1 (times n (factorial (sub1 n)))))
;;     What would happen if we used this version of factorial?  Why?
;;     Fortunately, Franz LISP has a built-in function called "if" that does
;;     just what we want.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Both the "then-action" and "else-action" will be evaluated before the "if"
; function is called.  As factorial is a recursive method, it will simply
; recurse infinitely in the false case, before "if" even gets invoked once.
