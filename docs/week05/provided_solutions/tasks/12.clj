(ns tasks.12
  (:require [clojure.java.io :as io]))


;Task 1
(defn chain [list current]
  "A version of the chain pattern.
   Arguments:
     - an initial value to be passed to the first lambda in the list
     - a list of lambdas that are to be executed in the order listed 
   Complexity:
     - O(n*u) where u is the largest complexity of each of the lambdas 
   Value:
     - a single value or collection of values resulting from the execution of all the lambdas"

  (if (empty? list)
    ;in this case, the value of chain is the current calculation
    current
    ;in this case, the value of chain is the result of recursion using as parameters the list's tail
    ;and executing lambda in the list with the current parameter
    (recur (rest list) ((first list) current))))

(defn speed-test []
  "A function to evaluate the speed of chain relative to reduce/comp
To be even close to accurate, standard speed testing behaviors 
such as disconnecting from any network, closing all other applications,
turning off background processes, etc. would have to be done.
   
   The links of the chain, chain-links, are the lambdas in desired execution order."
  ;I have chosen lambdas that don't do any calculation so that the lambdas' execution 
  ;doesn't mask any differences in speed between the custom and standard implimentations.
  ;By returning a distinct value regardless of the parameter passed, I know the order the 
  ;lambdas were called. 
  (let [chain-links [(fn [n] 1) (fn [n] 2) (fn [n] 3)
                     (fn [n] 4) (fn [n] 5) (fn [n] 6)
                     (fn [n] 7) (fn [n] 8) (fn [n] 9)]]
    (.write *out* "custom chaining:\n\t")
    (time (chain chain-links 100))
    (.write *out* "standard chaining:\n\t")
    ;the comp=>compose function, when used as the lambda for reduce, reduces a vector of functions [f1 f2 f3] as f3(f2(f1(params)))
    (time ((reduce comp (reverse chain-links)) 100))))

;Task 2
(defn execute []
  "An exercising function used to evaluate
   sales by year and region for a ficticious company"
  (let [regions ['europe 'north_america 'central_america 'south_america 'africa 'south_asia 'east_asia]
        years (range 2017 2022)
        ;generate a bunch of data as if it had been pulled from a database
        all-sales (for [m (range 1 13) y years r regions]
                    {:month m :year y :region r :sales-amount (rand-int 10000000)})

        lambdas [(fn [ungrouped-sales] ;group the sales by region and year, 
                   (for [r regions y years] {:region r :year y :sales-by (filter #(and (= (:region %) r) (= (:year %) y)) ungrouped-sales)}))
                 (fn [grouped-sales] ;get the total by region and year
                   (for [g grouped-sales] {:region (:region g) :year (:year g) :total (reduce #(+ (:sales-amount %2) %1) 0 (:sales-by g))}))]]
    ;I've chosen to use the standard chaining.
    ((reduce comp (reverse lambdas)) all-sales)))
