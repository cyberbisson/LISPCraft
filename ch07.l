;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LISPCraft (by Robert Wilensky)
;; Excercies from Chapter 7 (Property Lists)
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
;; (1) Use property lists to represent information about the cost and model
;;     number of a set of different makes of automobiles.  For example, you
;;     might include that a Mercedes 380SL costs $45,000, a BMW 320i, $15,000, a
;;     Plymouth Volare, $8,000, and a Toyota Tercel, $6,000.  Now write a
;;     function that, given a list of cars as input, returns the model of the
;;     least expensive car.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; NOTE: putprop/defprop don't exist in Common LISP.  The syntax is:
;   (setf (get 'atom :property) 'newvalue

(defun find-least-expensive-car-recursive (car-list result-price result)
  "Tail-recursive invocation of find-least-expensive-car"

  (if (null car-list) result

    (let* ((cur-car   (car car-list))
           (cur-price (get cur-car :price)))

      (if (and (not (null cur-price))
               (or  (null result-price)
                    (< cur-price result-price)))

          (find-least-expensive-car-recursive
           (cdr car-list)
           cur-price
           cur-car)

          (find-least-expensive-car-recursive
           (cdr car-list)
           result-price
           result)))))

(defun find-least-expensive-car (car-list)
  "Return the item in the list with the lowest value defined for the :price
 property."

  (find-least-expensive-car-recursive car-list nil nil))

(defun sanity-test-7-1 ()
  (let ((car-list '(Mercedes-380SL
                    BMW-320i
                    Tonka-Truck
                    Mystery-Box
                    Plymouth-Volare
                    Toyota-Tercel)))

    ; Set car prices...
    (setf (get 'Mercedes-380SL  :price) 45000)
    (setf (get 'BMW-320i        :price) 15000)
    (setf (get 'Tonka-Truck     :price) 15)
    (setf (get 'Plymouth-Volare :price) 8000)
    (setf (get 'Toyota-Tercel   :price) 6000)

    (sanity-check-result (find-least-expensive-car car-list) 'Tonka-Truck)
    (sanity-check-result
     (find-least-expensive-car (reverse car-list))
     'Tonka-Truck)
    (sanity-check-result
     (find-least-expensive-car (remove 'Tonka-Truck car-list))
     'Toyota-Tercel)

    (sanity-check-result (find-least-expensive-car '()) nil)
    (sanity-check-result (find-least-expensive-car '(foo bar)) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (2) A common use of property lists is to "mark" atoms for various purposes.
;;     For example, we can use the "marking" to speed up the set intersection
;;     function we wrote as an exercise in the last chapter (recall that set
;;     intersection takes two lists, and returns the elements that they have in
;;     common).  The idea is to go through one list of elements and "mark" them
;;     by putting something on their property lists.  For example, we can set
;;     the "mark" property of each atom to t.  Then we can go through the second
;;     list, and collect only those elements whose "mark" property is t, and
;;     return these as the set intersection.
;;
;;     This function is fast because it requires running through each list only
;;     once, while the version you wrote at the end of the last chapter had to
;;     run through one list once for every element of the other list.
;;
;;     Write a version of set intersect that uses property lists as described
;;     above.  Remember that we want elements to appear only once in the answer,
;;     no matter how many times they appear in the input.
;;
;;     If you are not careful, your version of this function may leave marks on
;;     atoms when it is finished.  Then subsequent calls may mistake these old
;;     marks for current markings and give the wrong answer.  Be sure that your
;;     version of set intersection does not behave this way.
;;
;;   - Note that this hopped up version of set intersection is intrinsically
;;     more limited than the previous version.  What is this limitation?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Limitations:
;  - Someone else could use the same property name, and we could really hose
;    each other.
;  - This isn't really thread-safe (if there is such a thing in LISP).

; I guess just mark every element in list-1, then gather the list-2 elements
; that are marked...

(defun marked-intersect-mark-all (input value)
  "Mark all elements in input with the given value."

  (do ((input-rest (cdr input) (cdr input-rest))
       (elem       (car input) (car input-rest)))
      ((null elem) input) ; This breaks if nil is in the list...
      (setf (get elem :intersect-mark) value)))

(defun marked-intersect-find-marked (input result)
  "Tail-recursive call to build a list of marked elements."

  (if (null input)
      (reverse result)

    (let* ((elem (car input))
           (marked (get elem :intersect-mark)))

      ;; Unmark to prevent duplicates
      (setf (get elem :intersect-mark) nil)

      (marked-intersect-find-marked
       (cdr input)
       (if marked (cons elem result) result)))))

(defun marked-intersect (list-1 list-2)
  "Using properties (:intersect-mark) to determine if an element exists in both
 lists."

  (marked-intersect-mark-all list-1 t)
  (let ((result (marked-intersect-find-marked list-2 '())))

    (marked-intersect-mark-all list-1 nil)
    result))

(defun sanity-test-7-2 ()
  (sanity-check-result (marked-intersect '(a b c d e) '(a b c d e)) '(a b c d e))

  ; FAILED -- No particular order.
  (sanity-check-result (marked-intersect '(a b b c d) '(c a b b))   '(a b c))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (3) We can implement a sort of virtual property list using a technique known
;;     as inheritance.  For example, suppose we stated that a canary was a bird,
;;     and that a bird has wings.  We would like to be able to ask the question,
;;     "Does a canary have wings?" and get a positive answer.  One way to do 
;;     this is to store these facts as properties.  For example, we can assert
;;     (defprop bird t wings), meaning that birds have wings, and (defpropr
;;     canary bird isa), meaning that a canary is a bird.  This latter assertion
;;     is sometimes called an "isa-link".
;;
;;     We now need a special function "inherit-get."  inherit-get will get two
;;     arguments, just like "get."  It will try to do a get first, and if this
;;     succeeds, will return the value "get" returns..  However, if get fails,
;;     inherit-get will use get to get the isa property of its first argument.
;;     If it finds one, inherit-get will examine the resulting value to see if
;;     it has the desired information.  inherit-get will keep going up this
;;     "isa heirarchy" until it finds the property requested, or it runs out of
;;     isa-links.
;;
;;     For example, suppose we evaluated (inherit-get 'canary 'wings).
;;     inherit-get will first tyy a "get" of the "wings" property of "canary."
;;     When this returns nil, inherit-get will do a get of the isa property of
;;     canary.  This returns "birt."  inherit-get then does a get of the wings
;;     property of bird, and gets the answer, t.  inherit-get returns this as
;;     its result.
;;
;;     In general, inherit-get may go through any number of iterations before it
;;     fails or finds an answer.  For example, if we evaluated (inherit-get
;;     'canary 'alive), we would first check canary, and then bird for an alive
;;     property.  These both fail, so we check the isa property of bird and find
;;     that it isa animal.  This may not have the desired information either, so
;;     we move up the heirarchy again.  The isa property of animal might be
;;     living-ling, which should carry the target information.
;;
;;     Write a version of inherit-get that works as described.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Ahem, this doesn't work very well if a property is false (we cannot
; differentiate between the property being false and undefined), or if a child
; has a false value for a property that is true in the parent...

(defun inherit-get (elem prop)
  "Traverse a chain of property inheritance until we find a value or an end to
 the chain."

  (let ((gotten (get elem prop)))

    (cond
     (gotten gotten)

     (t (let ((next-isa (get elem :isa)))
          (if (null next-isa) nil
            (inherit-get next-isa prop)))))))

(defun sanity-test-7-3 ()

  ; Set properties of various animal states
  (setf (get 'living-thing :alive) t)
  (setf (get 'bird         :wings) t)
  (setf (get 'finch        :color) 'brown)
  (setf (get 'canary       :color) 'yellow)

  ; Set "isa" relationships
  (setf (get 'animal :isa) 'living-thing)
  (setf (get 'plant  :isa) 'living-thing)
  (setf (get 'bird   :isa) 'animal)
  (setf (get 'finch  :isa) 'bird)
  (setf (get 'canary :isa) 'finch)

  (sanity-check-result (inherit-get 'canary :color) 'yellow)
  (sanity-check-result (inherit-get 'canary :wings) t)
  (sanity-check-result (inherit-get 'canary :alive) t)
  (sanity-check-result (inherit-get 'canary :dead)  nil)
  (sanity-check-result (inherit-get 'finch  :color) 'brown)
  (sanity-check-result (inherit-get 'animal :alive) t)
  (sanity-check-result (inherit-get 'animal :color) nil))
