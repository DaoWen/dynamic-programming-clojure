(ns stock.k-shot)

(declare ^:dynamic bps) ; buy prices
(declare ^:dynamic sps) ; sell prices

(defn append [xs x x-ret]
  (-> xs
    (update-in [:strategy] conj x)
    (update-in [:return] + x-ret)))

(defn ^:dynamic opt [j k]
  (if (or (<= j 1) (= k 0)) {:strategy [] :return 0}
    (->>
      (for [i (range 1 j)]
        (list (opt i k)
              (append (opt (dec i) (dec k))
                      [i j] (- (sps j) (bps i)))))
      (apply concat)
      (apply max-key :return))))

(defn find-opt [k bps sps]
  (binding [bps (comp bps dec), sps (comp sps dec)]
    (opt (count bps) k)))

(defn find-opt-memoized [k bps sps]
  (binding [opt (memoize opt)] (find-opt k bps sps)))

(defn rand-ints [x] (vec (repeatedly x #(rand-int 100))))

(let [k 5, n 10, bps (rand-ints n), sps (rand-ints n)]
  (println "bps:" bps)
  (println "sps:" sps)
  (find-opt-memoized 5 bps sps))
; bps: [88 41 45 89 32 20 56 60 49 58]
; sps: [42 66 29 47 72 61 86 59 85 89]
; {:strategy [[2 5] [6 7] [9 10]], :return 137}
 
(find-opt-memoized 5 [10 10] [1 1])
; => {:strategy [], :return 0}
