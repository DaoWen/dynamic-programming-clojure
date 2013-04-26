(ns indi-set.max-weight)

(declare ^:dynamic ws)

(defn get-weight [xs] (:weight (meta xs)))

(defn append [xs x]
  (with-meta (conj xs x) {:weight (+ (get-weight xs) (ws x))}))

(defn ^:dynamic opt [i]
  (if (neg? i) (with-meta [] {:weight 0})
    (let [optA (opt (dec i))
          optB (append (opt (- i 2)) i)]
      (max-key get-weight optA optB))))

(defn find-opt [weights]
  (binding [ws weights]
    (let [res (opt (dec (count ws)))]
      {:vs res :total-weight (get-weight res)})))

(defn find-opt-memoized [ws]
  (binding [opt (memoize opt)] (find-opt ws)))

(find-opt-memoized [1 8 6 3 6])
; => {:vs [1 4], :total-weight 14}

(find-opt-memoized [10 1 1 10])
; => {:vs [0 3], :total-weight 20}

(find-opt-memoized [1 10 15 10 1])
; => {:vs [1 3], :total-weight 20}

; This shows memoized version is ~10,000x faster
; than the naive recursive version even for n=32.
(let [ws (vec (repeatedly 32 #(rand-int 100)))]
  (println "ws:" ws)
  (time (find-opt-memoized ws))
  (time (find-opt ws)))
; ws: [57 57 76 82 63 45 83 78 52 48 61 47 95 80 34 6
;      46 87 50 21 16 56 99 10 30 55 18 15 61 62 54 0]
; "Elapsed time: 0.747 msecs"
; "Elapsed time: 6514.753 msecs"
; => {:vs [0 2 4 6 8 10 12 14 16 18 20 22 25 28 30], :total-weight 902}
