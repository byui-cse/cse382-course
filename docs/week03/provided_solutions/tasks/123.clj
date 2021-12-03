;; @author Lee Barney
;; @copyright 2021 Lee Barney licensed under the <a>
;;        rel="license"
;;        href="http://creativecommons.org/licenses/by/4.0/"
;;        target="_blank">
;;        Creative Commons Attribution 4.0 International License</a>
;;
;;
;; These solutions are not intended to be ideal solutions. Instead,
;; they are solutions that you can compare against yours to see
;; other options and to come up with even better solutions.
;;
(ns tasks.123
  (:require [clojure.string :as str])
  (:gen-class))

;tasks 1, 2, and 3
(defn c_map [list,predicate]
"A version of the map functor pattern.
   Arguments:
     - a list of any type of elements that is to be folded into 1 value
 	   - a lambda used to modify each element of the list
   Complexity:
     - O(n*u) where u is the complexity of the predicate
   Value:
     - a list of modified elements"
  (for [x list] (predicate x)))

(defn encipher [sequence]
"A function used to exercise the c_map function. It does a simple 'shift one' Caesar cipher."
  (c_map sequence #(+ (int %1) 1)))

(defn decipher [sequence]
"A function used to exercise the c_map function. 
It does a simple 'unshift one' to produce the original text."
  (apply str (c_map sequence #(char (- (int %1) 1)))))


(defn exercise_123 [text]
"A function used to check the accuracy of the encipher-decipher process."
		(decipher (encipher (seq text))))

(defn speed_test []
"A function to evaluate the speed of c_map relative to lists:map.
To be even close to accurate, standard speed testing behaviors 
such as disconnecting from any network, closing all other applications,
turning off background processes, etc. would have to be done."
		(let [nums (range 10000000)]
  		(print "custom map:\n\t")
  		;;The time function is used to find and print the amount of time required to run a function.
  		(time (c_map nums #(+ %1 1)))
  		(print "standard map:\n\t")
  		(time(map #(+ %1 1) nums))))