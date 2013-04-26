(ns vote.gerrymandering)

(declare ^:dynamic xs)

(defn ^:dynamic opt [i w k]
  (cond
    (zero? k) 0
    (zero? i) Double/NEGATIVE_INFINITY
    :else (let [opt1 (opt (dec i) w k)
                wi   (xs i)
                opt2 (when (>= w wi)
                       [(+ wi (opt (dec i) (- w wi) (dec k)))])]
            (apply max opt1 opt2))))

(defn opt-memoized [i w k]
  (binding [opt (memoize opt)] (opt i w k)))

(defn gerrymanderable? [{a-votes :a b-votes :b :as votes}]
  (let [n (count a-votes)
        m (+ (a-votes 0) (b-votes 0))
        k (/ n 2)
        half-votes (/ (* n m) 2)
        w (dec (/ half-votes 2))
        a-total (reduce + a-votes)
        x (if (> half-votes a-total) :a :b)
        xs (x votes)
        minority-total (reduce + xs)
        district1 (binding [xs (comp xs dec)] (opt-memoized n w k))
        district2 (- minority-total district1)]
    [x district1 district2 '<= w (>= w district2)]))

(gerrymanderable?
  {:a [55 43 60 47]
   :b [45 57 40 53]})

(defn rand-ints [x lim] (vec (repeatedly x #(rand-int lim))))

(dotimes [i 1000]
  (let [n 100
        a-votes (rand-ints 10 n)
        b-votes (mapv #(- n %) a-votes)
        [x _ _ _ _ yes?] (gerrymanderable? {:a a-votes :b b-votes})]
    (when-not yes?
      (when-not (= (apply + a-votes) 500)
        (println x a-votes b-votes
                 (Math/abs (- (apply + a-votes) (apply + b-votes))))))))

(->> x
  (iterate shuffle)
  (filter #(= (apply + (take 5 %)) 248))
  first
  (partition 5)
  (map (juxt identity (partial apply +))))

