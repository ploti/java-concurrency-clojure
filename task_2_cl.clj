; Algorithm: https://web.archive.org/web/20150710134640/http://diditwith.net/2009/01/20/YAPESProblemSevenPart2.aspx

(defn get-all-primes
  []
  (letfn [(reinsert [table x prime]
             (update-in table [(+ prime x)] conj prime))
          (primes-step [table d]
             (if-let [factors (get table d)]
               (recur (reduce #(reinsert %1 d %2) (dissoc table d) factors)
                      (inc d))
               (lazy-seq (cons d (primes-step (assoc table (* d d) (list d))
                                              (inc d))))))]
    (primes-step {} 2)))

    (take 2 (get-all-primes))
    
    (take 5 (get-all-primes))
    
    (take 9 (get-all-primes))
    
    (take 12 (get-all-primes))
    
    (take 22 (get-all-primes))