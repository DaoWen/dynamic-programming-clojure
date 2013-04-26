(ns lines.pretty-print
  (use [clojure.string :only (split)]))

(declare ^:dynamic cs)

(declare ^:dynamic max-len)

; "sss" is "sum of squares slack"
(defn get-sss [xs] (:sss (meta xs)))

(defn append [xs x x-sss]
  (with-meta (conj xs x) {:sss (+ (get-sss xs) x-sss)}))

(defn sqr [x] (* x x))

(defn all-slacks [j]
  (let [slack0 (- max-len (cs j))]
    (loop [i (dec j), slack slack0, acc [[j (sqr slack0)]]]
      (cond
        (neg? slack) (pop acc)
        (neg? i) acc
        :else (let [slack' (- slack (cs i) 1)]
                (recur (dec i) slack' (conj acc [i (sqr slack')])))))))

(defn ^:dynamic opt [j]
  (if (neg? j) (with-meta [] {:sss 0})
    (let [opts (for [[i sss] (all-slacks j)]
                 (append (opt (dec i)) i sss))]
      (apply min-key get-sss opts))))

(defn pp-lines [s l]
  (let [words (split s #"\s+")
        parts (binding [cs (vec (map count words)), max-len l]
                (opt (dec (count words))))]
    (loop [n 0, ws words, [p & ps] (rest parts), acc []]
      (if (nil? p) (conj acc ws)
        (let [[h t] (split-at (- p n) ws)]
          (recur (+ n (count h)) t ps (conj acc h)))))))

(defn pretty-print-str [s len]
  (doseq [l (pp-lines s len)] (apply println l)))

(defn ppstr-memo [s l]
  (binding [opt (memoize opt)] (pretty-print-str s l)))

(pretty-print-str "Hello, my name is Nick!" 10)

(def moby-dick
  "Call me Ishmael. Some years ago, never mind how long precisely, having
little or no money in my purse, and nothing particular to interest me on
shore, I thought I would sail about a little and see the watery part of
the world.")

(ppstr-memo moby-dick 40)

(binding [max-len 11
          cs [1 1 3 4 5 6]]
  (opt 5))
