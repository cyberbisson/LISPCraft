;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LISPCraft (by Robert Wilensky)
;; Excercies from Chapter 8 (Functions as Arguments: EVAL, APPLY, and Mapping)
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
;; (1) What are the values of the following expressions:
;;     (a) (eval (list 'car '(cdr '(b c))))
;;     (b) (eval (list 'car ''(cdr '(b c))))
;;     (c) (eval (cons 'cdr '('(a b c))))
;;     (d) (apply 'cdr '((a b c)))
;;     (e) (mapcar 'list '(a b) '(c d))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a: (eval '(car (cdr '(b c)))) => C
; b: (eval '(car '(cdr '(b c)))) => (car '(cdr '(b c))) => CDR
; c: (eval '(cdr '(a b c))) => (B C)
; d: (cdr '(a b c)) => (B C)
; e: ((A C) (B D))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (2) Write a version of the function "make-assoc-list" using mapping
;;     functions.  Recall that this function makes a list of the sort "assoc"
;;     uses out of two lists of items to be paired with one another.  For
;;     example, (make-assoc-list '(a b c d) '(1 2 3 4)) returns
;;     ((a 1) (b 2) (c 3) (d 4)).  Compare this version to the iterative version
;;     you wrote for the exercise at the end of Chapter 6.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-make-assoc-list-8 (keys values)
  "Simple wrapper around mapcar to associate key/value lists."

  (mapcar 'list keys values))

(defun sanity-test-8-2 ()
  (sanity-check-result (my-make-assoc-list-8 '(a b c d) '(1 2 3 4))
                       '((a 1) (b 2) (c 3) (d 4)))

  (sanity-check-result (my-make-assoc-list-8 '(a b c) '(1 2 3 4))
                       '((a 1) (b 2) (c 3)))

  (sanity-check-result (my-make-assoc-list-8 '(a b c d) '(1 2 3))
                       '((a 1) (b 2) (c 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (3) Write your own version of "mapcar," or of any of the other mapping
;;     functions, using "prog" and "eval" or "apply."  Your version will have to
;;     be more limited than the actual LISP function, since we do not yet know
;;     how to write functions that accept a variable number of arguments, as
;;     does the real "mapcar."  So write your version assuming your function
;;     argument will always be a function of exactly one argument.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-mapcar (func params)
  "Provide a simplistic implementation of MAPCAR that maps only one list of
 parameters."

  (prog (result params-left)
        (setq params-left params)

        loop
        (cond ((endp params-left) (return (nreverse result)))) ; REVERSE AGAIN!

        (setq result (cons (apply func (list (car params-left))) result))
        (setq params-left (cdr params-left))
        (go loop)))

(defun sanity-test-8-3 ()
  (sanity-check-result (my-mapcar 'list '(a b c d e))
                       (mapcar    'list '(a b c d e)))

  (sanity-check-result (my-mapcar 'not '(t t nil t nil))
                       (mapcar    'not '(t t nil t nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (4) Use a mapping function to write a better version of the matrix transpose
;;     function described in the exercises in Chapter 6.  Recall that we
;;     represented a matrix as a list of lists; the transpose of ( (a b) (c d) )
;;     would be ( (a c) (b d) ).  (Hint - the most concise version involves
;;     "apply"ing a mapping function.)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun transpose-8 (matrix)
  nil ; hate hate hate this one.
)

(defun sanity-test-8-4 ()
  (sanity-check-result
   (transpose-8
    '((a b)
      (c d)))
   '((a c)
     (b d)))

  (sanity-check-result
   (transpose-8
    '((1 2)
      (3 4)
      (5 6)))
   '((1 3 5)
     (2 4 6)))

  (sanity-check-result
   (transpose-8
    '((1 3 5)
      (2 4 6)))
   '((1 2)
     (3 4)
     (5 6))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (5) Using mapping functions, rewrite the set intersection function described
;;     at the end of Chapter 6.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun intersect-8 (list-1 list-2)
  nil)

(defun sanity-test-8-5 ()
  (sanity-check-result (intersect-8 '(a b b c d) '(c a b b)) '(a b c))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (6) Suppose a function returns a list of true or false values, e.g.,
;;     (nil nil t nil t nil).  Suppose we want to know if this list contains any
;;     non-nil value.  Use a mapping function in conjunction with a logical
;;     operator to make this determination.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flag-answer (to-test)
  "Simple function to set a global if we encounter a true value.  This must be
 set to nil manually."

  (if to-test (setf (get 'contains-t-answer :value) t)))

(defun contains-t (booleans)
  "Check a list and return t if any values in the list are true."

  ; Should probably check here if the flag is true -- we're definitely stepping
  ; on another thread, if so...

  (mapc 'flag-answer booleans)

  ; This is just to reset my global property on completion...
  (let ((retval (get 'contains-t-answer :value)))
    (setf (get 'contains-t-answer :value) nil)

    retval))

(defun sanity-test-8-6 ()
  (sanity-check-result (contains-t '(nil nil t nil t nil)) t)
  (sanity-check-result (contains-t '(nil nil t nil nil)) t)
  (sanity-check-result (contains-t '(t nil t nil)) t)
  (sanity-check-result (contains-t '()) nil)
  (sanity-check-result (contains-t '(nil nil nil nil)) nil)
  (sanity-check-result (contains-t '(t t)) t)
  (sanity-check-result (contains-t '(t)) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (8) One problem with the various mapping functions is that there is no way to
;;     stop them before they have run to completion.  In many applications, we
;;     want to apply a function to arguments repeatedly until a certain
;;     condition is true.  Therefore, many LISPs include some function applying
;;     functions that apply their function only as long as it is necessary to do
;;     so.  Unfortunately, Franz does not contain such functions.  But we now
;;     have the tools to create them ourselves.
;;
;;     One useful function that combines flow of control and function mapping is
;;     called "some."  This is a function of two arguments, which should
;;     evaluate to a function and a list.  It applies the function to successive
;;     elements of the list until the function returns non-nil.  Then it returns
;;     the elements of the list from that point on.  It returns nil otherwise.
;;     For example, (some 'numbp '(a b 2 c d)) should return (2 c d).
;;
;;     The function "every" is like "some," except that it stops as soon as one
;;     of the function applications returns nil.  every then returns nil as its
;;     value.  If all the applications return non-nil, every returns t.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-every (func input)
  "Return t if func returns a non-nil value for every element in input.
 Otherwise, return nil."

  (cond
   ((endp input)                      t)
   ((null (funcall func (car input))) nil)
   (t
    (my-every func (cdr input)))))

(defun my-some (func input)
  "Execute func on a list until it returns a non-nil value.  This function then
 returns the remainder of the list (including the value that caused the non-nil
 result)."

  (cond
   ((null input)                            nil)
   ((not (null (funcall func (car input)))) input)
   (t                                       (my-some func (cdr input)))))

(defun sanity-test-8-7 ()
  (sanity-check-result (my-some 'numberp '(a b 2 c d)) '(2 c d))
  (sanity-check-result (my-some 'numberp '(1 a b 2 c d)) '(1 a b 2 c d))
  (sanity-check-result (my-some 'numberp '(a b c d 1)) '(1))
  (sanity-check-result (my-some 'numberp '(a b c d)) nil)
  (sanity-check-result (my-some 'numberp nil) nil)
  (sanity-check-result (my-some 'null '(1 2 3 4 nil)) '(nil))

  (sanity-check-result (my-every 'numberp '(1 2 a 3 4)) nil)
  (sanity-check-result (my-every 'numberp '(a 1 2 b 3 4)) nil)
  (sanity-check-result (my-every 'numberp '(1 2 3 4 a)) nil)
  (sanity-check-result (my-every 'numberp '(1 2 3 4)) t)
  (sanity-check-result (my-every 'null '(1 2 3 4 nil)) nil)
  (sanity-check-result (my-every 'numberp nil) nil) ; Fails - recursion not
                                                    ; handling initial values
                                                    ; well...
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (8) Define a function "subset" that takes two arguments, which should
;;     evaluate to a function and a list.  subset should apply the function to
;;     successive elements of the list.  It returns a list of all the elements
;;     of this list for which the function application returns non-nil.  For
;;     example, (subset 'numberp '(a b 2 c d 3 e f)) returns (2 3).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun subset-recurse (result func input)
  "Tail-recursive call for SUBSET."

  (if (endp input) (nreverse result) ; REVERSE!

    (let ((elem (car input)))
      (subset-recurse
       (if (null (funcall func elem))
           result
           (cons elem result))
       func (cdr input)))))

(defun subset (func input)
  "Create a list that is a subset of INPUT that contains all elements where FUNC
 returns a non-nil value."

  (subset-recurse '() func input))

(defun sanity-test-8-8 ()
  (sanity-check-result (subset 'numberp '(a b 2 c d 3 e f)) '(2 3))
  (sanity-check-result (subset 'numberp '(a b c d e f)) nil)
  (sanity-check-result (subset 'numberp '(1 2 3 4)) '(1 2 3 4))
  (sanity-check-result (subset 'numberp nil) nil)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (9) We can use "eval" and "apply" to implement a programming technique called
;;     "data-driven" programming.  In this technique, we store functions along
;;     with data, and use the data to suggest which functions to use to
;;     manipulate the data.
;;
;;     For example, suppose we had a data base of assorted objects, each of
;;     which we want to display according to different rules.  People in the
;;     data base might be identified by first and last names, books by title and
;;     author, cars by make, year, and model, etc...  We could put the code do
;;     do this inside a big "cond," but then we would have to hack this code in
;;     order to extend our function to other types of data.  A more elegant
;;     solution is to first write separate functions for each type of datum:
;;       -> (defun car-display (datum)
;;            (list (get datum 'year)
;;                  (get datum 'make)
;;                  (get datum 'model)))
;;       -> (defun book-display (datum)
;;            (list (get datum 'title) 'by (get datum 'author)))
;;     and so on.  Now we attach one such function to the "display-fn" property
;;     of each datum:
;;       -> (defprop car13 car-display display-fn)
;;       -> (defprop book7 book-display display-fn)
;;     etc.  Of course, we would need to define the other properties of these
;;     data as well.
;;
;;     Now we can write a generic display function, "item-display."  This
;;     function picks the display function off the property list of the item to
;;     be displayed, and applies it to that item.  We can extend our displaying
;;     capability to new types of data items without modifying this function at
;;     all.  Instead, we simply write a new display function for the new data
;;     type and put it on the property list of each item of that data type.
;;
;;     Write the function "item-display," and use it to display a small data
;;     base of objects.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun item-display (datum)
  "Display items using the :DISPLAY-FN property if possible."

  (let ((disp-func (get datum :display-fn)))
    (if (null disp-func) datum ; Just return the item if we have no display fn
      (funcall (get datum :display-fn) datum))))

(defun book-display (datum)
  "Display the properties of a book."
  (list (get datum :title) 'by (get datum :author)))

(defun car-display (datum)
  "Display the properties of a car."
  (list (get datum :year)
        (get datum :make)
        (get datum :model)))

(defun sanity-test-8-9 ()

  ;; Books
  (setf (get 'book1 :display-fn) 'book-display)
  (setf (get 'book1 :author)     "Sinclair Lewis")
  (setf (get 'book1 :title)      "Oil!")

  (setf (get 'book2 :display-fn) 'book-display)
  (setf (get 'book2 :author)     "F. Scott Fitzgerald")
  (setf (get 'book2 :title)      "The Great Gatsby")

  (setf (get 'book3 :display-fn) 'book-display)
  (setf (get 'book3 :author)     "Miyamoto Musashi")
  (setf (get 'book3 :title)      "The Book of Five Rings")

  ;; Cars
  (setf (get 'car1 :display-fn) 'car-display)
  (setf (get 'car1 :year)       2007)
  (setf (get 'car1 :make)       'Chevrolet)
  (setf (get 'car1 :model)      'Corvette)

  (setf (get 'car2 :display-fn) 'car-display)
  (setf (get 'car2 :year)       1992)
  (setf (get 'car2 :make)       'Geo)
  (setf (get 'car2 :model)      'Prism)

  ;; Hasty "foreach"...
  (mapcar 'item-display '(book1 book2 book3 car1 car2)))
