;     Program: exercises.clj
;      Author: Steven Tomcavage (stomcava@seas.upenn.edu)
; Description: Getting started with Clojure
;        Date: October, 2010

(defn shallow-reverse 
  "Reverses the top level elements of lst"
  ([lst] 
    (shallow-reverse () lst))
  ([reversed lst]
    (cond
      (empty? lst) reversed
      :else (recur (cons (first lst) reversed) (rest lst)))))

(defn member?
  "Tests whether a value is a member of the given sequence"
  ([lst value]
    (cond
      (empty? lst) false
      (= (first lst) value) true
      :else (recur (rest lst) value))))

(defn remove-duplicates 
  "Removes duplicate elements of lst" 
  ([lst]
    (remove-duplicates () lst))
  ([deduped lst]
    (cond
      (empty? lst) (reverse deduped)
      (member? deduped (first lst)) (recur deduped (rest lst))
      :else (recur (cons (first lst) deduped) (rest lst)))))

(defn my-flatten 
  "Returns lst with all inner parentheses removed" 
  ([lst] 
    (my-flatten () lst))
  ([flattened lst]
    (cond
      (empty? lst) (reverse flattened)
      (seq? (first lst)) (recur (concat (reverse (my-flatten (first lst))) flattened) (rest lst))
      :else (recur (cons (first lst) flattened) (rest lst)))))

(defn skeleton 
  "Removes all non-sequence elements of lst, but retains sequence structure" 
  ([lst] 
    (skeleton () lst))
  ([frame lst]
    (cond
      (empty? lst) (reverse frame)
      (seq? (first lst)) (recur (cons (skeleton (first lst)) frame) (rest lst))
      :else (recur frame (rest lst)))))

(defn deep-reverse 
  "Reverses the elements of lst at all levels" 
  ([lst] 
    (deep-reverse () lst))
  ([reversed lst]
    (cond
      (empty? lst) reversed
      (seq? (first lst)) (recur (cons (deep-reverse (first lst)) reversed) (rest lst))
      :else (recur (cons (first lst) reversed) (rest lst)))))

(defn eliminate 
  "Returns lst with all occurances of value removed, at all levels" 
  ([value lst] 
    (eliminate () value lst))
  ([eliminated value lst]
    (cond
      (empty? lst) (reverse eliminated)
      (= (first lst) value) (recur eliminated value (rest lst))
      (seq? (first lst)) (recur (cons (eliminate value (first lst)) eliminated) value (rest lst))
      :else (recur (cons (first lst) eliminated) value (rest lst)))))

(defn ascii?
  "Test the given character to see if it is a printable ASCII character"
  [character]
  (let [int-c (int character)]
    (cond
      (= int-c 10) true
      (= int-c 13) true
      (and (>= int-c 32) (<= int-c 126)) true
      :else false)))

(defn zap-gremlins 
  "Returns text with all non-ASCII characters removed" 
  [text] 
  (apply str (filter ascii? text)))

(defn ascii-upper?
  "Test the given character to see if it is an upper case letter in ASCII"
  [character]
  (let [int-c (int character)]
    (cond
      (and (>= int-c 65) (<= int-c 90)) true
      :else false)))

(defn ascii-lower?
  "Test the given character to see if it is a lower case letter in ASCII"
  [character]
  (let [int-c (int character)]
    (cond
      (and (>= int-c 97) (<= int-c 122)) true 
      :else false)))

(defn rot-13 
  "Applies the rot-13 transformation to text" 
  [text] 
  (apply str (map 
               (fn [c] 
                 (cond 
                   (ascii-upper? c) (char (+ (mod (+ (- (int c) 65) 13) 26) 65))
                   (ascii-lower? c) (char (+ (mod (+ (- (int c) 97) 13) 26) 97))
                   :else c))
               text)))

(defn newton-closure
  "Returns a closure around the iterator used to compute sqrt n"
  [n]
  (fn [r] (/ (+ r (/ n r)) 2)))

(defn sqrt 
  "Returns the square root of n, found using Newton's method" 
  ([n]
    (user/sqrt n 2.0 (newton-closure n)))
  ([n r iterator]
    (let [r-pair (take 2 (iterate iterator r))]
      (let [r1 (first r-pair) r2 (second r-pair)]
        (cond
          (and (>= r1 r2) (< (- r1 r2) 0.00001)) r2
          (and (>= r2 r1) (< (- r2 r1) 0.00001)) r2
          :else (recur n r2 iterator))))))

(defn collatz
  "Determines the next number in the collatz sequence after n"
  [n]
  (cond 
    (= n 1) 1
    (even? n) (/ n 2)
    (odd? n) (inc (* 3 n))))

(defn collatz-length
  "Computes the length of the collatz sequence for n"
  [n]
  (inc (count (for [x (iterate collatz n) :while (not= x 1)] x))))

(defn longest-collatz 
  "Returns the value between lo and hi, inclusive, with the longest converging Collatz sequence" 
  ([lo hi]
    (longest-collatz (range lo (inc hi)) 0 0))
  ([lst longest length]
    (cond
      (empty? lst) longest
      (> (collatz-length (first lst)) length) (recur (rest lst) (first lst) (collatz-length (first lst)))
      :else (recur (rest lst) longest length))))

(defn unit-test
  "Runs a basic set of unit tests on the exercises in this program"
  []
  (println "shallow-reverse ok?" (= (shallow-reverse '(1 2 (3 4))) '((3 4) 2 1)))
  (println "remove-duplicates ok?" (= (remove-duplicates '(1 2 3 1 4 1 2)) '(1 2 3 4)))
  (println "my-flatten ok?" (= (my-flatten '(1 (2 3) () (()) :a)) '(1 2 3 :a)))
  (println "skeleton ok?" (= (skeleton '(1 (2 (3 4)) 5 6  (7) ( ))) '((( )) ( ) ( ))))
  (println "deep-reverse ok?" (= (deep-reverse '(:a (:b :c (:d)) :e)) '(:e ((:d) :c :b) :a)))
  (println "eliminate ok?" (= (eliminate :b '(:a :b (:b :c :d (:a (:b))) :e)) '(:a (:c :d (:a ())) :e)))
  (println "eliminate sequence ok?" (= (eliminate '(:a :b) '(:a :b (:a :b) :c)) '(:a :b :c)))
  (println "zap-gremlins ok?" (= (zap-gremlins (str (char 48) (char 49) (char 960))) "01"))
  (println "rot-13 ok?" (= (rot-13 "nowhere!ABJURER") "abjurer!NOWHERE"))
  (println "sqrt ok?" (and (= (user/sqrt 9) 3.0) (= (user/sqrt 25) 5.0)))
  (println "longest-collatz ok?" (= (longest-collatz 2 5) 3))
)
