(ns ru.nsu.fit.artur_ploter.java_concurrency.task_1_cl
	(:gen-class))

(defn permutations [l]
  (if (= 1 (count l))
    (list l)
    (for [head l
          tail (permutations (disj (set l) head))]
      (cons head tail))))

(defn comb [k l]
  (if (= 1 k) (map vector l)
      (map permutations (apply concat
             (map-indexed
              #(map (fn [x] (conj x %2))
                    (comb (dec k) (drop (inc %1) l)))
              l)))))

(comb 2 ["a" "b" "c"])

(comb 2 ["a" "b" "c" "d"])