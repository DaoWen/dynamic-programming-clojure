(ns zion.emp)

(declare ^:dynamic xs)
(declare ^:dynamic f)

(defn ^:dynamic opt [j]
  (reduce max 0
    (for [i (range j)]
      (+ (opt i) (min (xs j) (f (- j i)))))))

(defn find-opt [xs f]
  (binding [xs (comp xs dec), f (comp f dec)]
    (opt (count xs))))

(defn find-opt-memoized [xs f]
  (binding [opt (memoize opt)] (find-opt xs f)))

(find-opt-memoized [1 10 10 1] [1 2 4 8])
; => 5

(find-opt-memoized [1 3 7 15 31] #(* 2 %))
; => 31
