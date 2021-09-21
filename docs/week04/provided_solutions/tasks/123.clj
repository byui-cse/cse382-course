(ns tasks.123
  (:require [clojure.string :as str])
  (:gen-class))

;tasks 1, 2, and 3
(defn c-foldl [list accumulator fold-func]
  "A version of the fold left pattern.
   Arguments:
     - a list of any type of elements that is to be folded into 1 value
     - an initial value to which is merged the elements of the list
 	 - a function used to merge the accumulator and each element of the list
   Complexity:
     - O(n*u) where u is the complexity of the fold-func function
   Value:
     - a single value of some type"
    (if (empty? list)
     	accumulator
   		 (recur (rest list) (fold-func accumulator (first list)) fold-func)))

(defn build-TLAs []
  "A testing function to build Three Letter Acronyms."
		(let [chars (for [x (range 17 42)](char (+ x(int \0))))]
  					(split-at 20000 (shuffle (for [a chars
                                      b chars
                                      c chars]
                                  (apply str [a b c]))))))

(defn build-text []
  "A testing function to build a text string from a set of Three Letter Acronyms."
  		(c-foldl (build-TLAs) " " #(str/join " " [%1 %2])))

(defn speed-test []
  "A testing function to display the speed of c-foldl relative to the native reduce function."
		(let [TLAs (build-TLAs)]
			(print "custom foldl:\n\t")
			(time (c-foldl TLAs "" #(str/join " " [%1 %2])))
			(print "standard fold:\n\t")
			(time (clojure.string/trim (reduce #(str/join " " [%1 %2]) "" TLAs)))))

;tasks 4, 5, and 6
(defn c-foldr [list accumulator fold-func]
  (c-foldl (reverse list) accumulator fold-func))
"A version of the fold right pattern.
   Arguments:
     - a list of any type of elements that is to be folded into 1 value
     - an initial value to which is merged the elements of the list
 	 - a function used to merge the accumulator and each element of the list
   Complexity:
     - O(n*u) where u is the complexity of the fold-func function
   Value:
     - a single value of some type"


(defn r-build-text []
  "A testing function that builds a text string from the TLA's"
  (c-foldr (build-TLAs) " " #(str/join " " [%1 %2])))

(defn r-speed-test []
  "A testing function to display the speed of c-foldr relative to the native reduce function."
  (let [TLAs (build-TLAs)]
    (print "custom foldl:\n\t")
    (time (c-foldr TLAs "" #(str/join " " [%1 %2])))
    (print "standard fold:\n\t")
    (time (clojure.string/trim (reduce #(str/join " " [%1 %2]) "" (reverse TLAs))))))