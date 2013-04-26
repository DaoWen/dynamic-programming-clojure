(ns inventory.orders)

(declare ^:dynamic ds)
(declare ^:dynamic C)
(declare ^:dynamic K)
(declare ^:dynamic S)

(defn append
  ([xs x-cost] (update-in xs [:cost] + x-cost))
  ([xs x x-cost] (update-in (append xs x-cost) [:orders] conj x)))

(defn ^:dynamic opt [i d s]
  (if (zero? i) {:orders [] :cost 0}
    (let [opt1 (append (opt (dec i) 0 S) [i (+ (ds i) d)] (+ K (* C d)))
          opt2 (append (opt (dec i) (+ (ds i) d) (- s (ds i))) (* C d))]
      (apply min-key :cost opt1
             (when (and (> i 1) (<= (ds i) s)) [opt2])))))

(defn find-opt [ds C K S]
  (binding [ds (comp ds dec), C C, K K, S S]
    (opt (count ds) 0 S)))

(defn find-opt-memoized [ds C K S]
  (binding [opt (memoize opt)] (find-opt ds C K S)))

(find-opt-memoized [1 3 2 5 6 4] 1 10 10)
; => {:orders [[1 6] [4 15]], :cost 41}
;
(find-opt-memoized [1 3 2 5 6 4] 10 1 10)
; => {:orders [[1 1] [2 3] [3 2] [4 5] [5 6] [6 4]], :cost 6}

(find-opt-memoized [1 3 2 5 6 4] 1 10 5)
; => {:orders [[1 6] [4 5] [5 10]], :cost 41}
