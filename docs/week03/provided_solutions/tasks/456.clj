(ns tasks.456
  (:gen-class))

(defn c_filter [list,predicate]
  (for [x list :when (predicate x)] x))

(defn prime_filter []
		(let [nums (range 20000)
  						primes '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139)]
  				(c_filter nums #((= (mod %1 2) 0)))))

(defn speed_test[]
		(let [nums (range 20000)
								primes '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139)]
  		(print "custom map:\n\t")
  		(time (c_filter nums #((= (mod %1 2) 0))))
  		(print "standard map:\n\t")
  		(time(filter #((= (mod %1 2) 0)) nums))))