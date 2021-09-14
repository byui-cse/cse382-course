(ns tasks.123
  (:require [clojure.string :as str])
  (:gen-class))

;tasks 1, 2, and 3
(defn c_map [list,predicate]
  (for [x list] (predicate x)))

(defn encipher [sequence]
  (c_map sequence #(+ (int %1) 1)))

(defn decipher [sequence]
  (apply str (c_map sequence #(char (- (int %1) 1)))))


(defn exercise_123 [text]
		(decipher (encipher (seq text))))

(defn speed_test []
		(let [nums (range 10000000)]
  		(print "custom map:\n\t")
  		(time (c_map nums #(+ %1 1)))
  		(print "standard map:\n\t")
  		(time(map #(+ %1 1) nums))))