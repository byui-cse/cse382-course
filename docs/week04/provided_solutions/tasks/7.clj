;; @author Lee Barney
;; @copyright 2020 Lee Barney licensed under the <a>
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

(ns tasks.7)


(defn c-unfold [count state unfold-func]
  "A naive implementation of the unfold functor pattern. This version is 
   very limited and should not be used in production.
   Arguments:
     - the number of items to be generated
     - the current state of the unfold-process
     - a lambda function to apply to the combination of 
          the state and count that generates the next state and count
   Complexity:
     - O(n*u) where u is the complexity of the unfold-func
   Value: 
     - a list of elements of the same type as the initial state"
  (if (= count 0)
    '()
    (let [tuple (unfold-func count state)]
       (conj (c-unfold (:cur-count tuple) (:cur-state tuple) unfold-func) (:cur-count tuple)))))

(defn test-predicate [count state]
  "A simple, silly predicate designed for testing the c-unfold function.
   Arguments:
     - a natural number representing how many unfolds are left to be done
     - a natural number that is the state of the un-fold prior to applying this predicate function
   Complexity:
     - O(1)
   Value:
     - a tuple containing the updated count and state"
  {:cur-count (- count 1) :cur-state (+ state 1)})

(defn test-behavior []
  "A function to begin exercising the c-unfold function"
  (let [gen-list (c-unfold 50 0 test-predicate)]
    (and (= (count gen-list) 50) (= (reduce + 0 gen-list) 1225))))
