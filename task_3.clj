(ns ploter.clojure.task_3
  (:gen-class))

(use 'clojure.test)
(use 'clojure.test.junit)

(defn filter_parallel
  [condition coll]
  (if-not (empty? coll)
    (if (condition (first coll))
      (cons (first coll) (filter_parallel condition (rest coll)))
      (filter_parallel condition (rest coll)))))

(defn filter_promises
  [condition coll]
  (let [chunk-size (int (Math/ceil (Math/sqrt (count coll))))
        parts (partition-all chunk-size coll)]
    (flatten (map deref (doall (map (fn [coll1]
                                      (future (filter_parallel condition coll1))) parts))))))


(defn filter_promises_inf
  [condition coll]
  (if (empty? coll)
    '()
    (concat (filter_promises condition (take 100 coll))
            (lazy-seq (filter_promises_inf condition (drop 100 coll))))))


;;https://alvinalexander.com/java/jwarehouse/clojure/src/clj/clojure/core.clj.shtml
(defn long-even?
  "Returns true if n is even, throws an exception if n is not an integer"
  {:added "1.0"
   :static true}
  [n]
  (Thread/sleep 100)
  (if (integer? n)
    (zero? (bit-and (clojure.lang.RT/uncheckedLongCast n) 1))
    (throw (IllegalArgumentException. (str "Argument must be an integer: " n)))))


(deftest fenhuasa
  (is (= (filter even? [1 2 3 4]) (filter_promises even? [1 2 3 4])))
  (is (= (filter even? []) (filter_promises even? [])))
  (is (= 384 (reduce * (filter_promises long-even? (range 1 10))))))

(defn gen_number_seq
  ([] (gen_number_seq 1))
  ([n] (lazy-seq (cons n (gen_number_seq (inc n))))))

(run-tests 'ploter.clojure.task_3)
(time (reduce + (filter_promises long-even? (range 1 10)))) 
(time (reduce + (filter long-even? (range 1 10))))
(time (reduce + (take 20 (filter_promises_inf long-even? (gen_number_seq)))))