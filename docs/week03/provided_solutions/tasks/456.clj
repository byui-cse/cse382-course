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
(ns tasks.456
  (:gen-class))

(defn c_filter [list,predicate]
"A version of the filter functor pattern.
   Arguments:
     - a list of any type of elements that is to be folded into 1 value
 	   - a lambda used to determine inclusion/exclusion each element of the list
   Complexity:
     - O(n*u) where u is the complexity of the predicate
   Value:
     - a list of elements for which the predicate had a value of true"
  (for [x list :when (predicate x)] x))

(defn prime_filter []
"A function to exercise the c_filter function. the result is a list of prime numbers."
		(let [nums (range 20000)
  						primes '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139)]
  				(c_filter nums #((= (mod %1 2) 0)))))

(defn speed_test[]
"A function to evaluate the speed of c_map relative to lists:map.
To be even close to accurate, standard speed testing behaviors 
such as disconnecting from any network, closing all other applications,
turning off background processes, etc. would have to be done."
		(let [nums (range 20000)
								primes '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139)]
  		(print "custom map:\n\t")
  		;;The time function is used to find and print the amount of time required to run a function.
  		(time (c_filter nums #((= (mod %1 2) 0))))
  		(print "standard map:\n\t")
  		(time(filter #((= (mod %1 2) 0)) nums))))