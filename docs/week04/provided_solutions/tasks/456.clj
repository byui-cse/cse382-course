(ns tasks.456
  (:require [clojure.string :as str]
            [tasks.123 :as t123])
		
  (:gen-class))


;tasks 4, 5, and 6
(defn c-foldr [list accumulator fold-func]
  (t123/c-foldl (reverse list) accumulator fold-func))
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
  (t123/c-foldr (t123/build-TLAs) " " #(str/join " " [%1 %2])))

(defn r-speed-test []
  "A testing function to display the speed of c-foldr relative to the native reduce function."
    (let [TLAs (t123/build-TLAs)]
    (print "custom foldl:\n\t")
    (time (c-foldr TLAs "" #(str/join " " [%1 %2])))
    (print "standard fold:\n\t")
    (time (clojure.string/trim (reduce #(str/join " " [%1 %2]) "" (reverse TLAs))))))
