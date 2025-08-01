(ns main)

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare main_cat main_j main_n main_t)

(def main_n 15)

(def main_t [])

(defn -main []
  (dotimes [_ (+ main_n 2)] (def main_t (conj main_t 0)))
  (def main_t (assoc main_t 1 1))
  (doseq [i (range 1 (+ main_n 1))] (do (def main_j main_i) (while (> main_j 1) (do (def main_t (assoc main_t main_j (+ (nth main_t main_j) (nth main_t (- main_j 1))))) (def main_j (- main_j 1)))) (def main_t (assoc main_t (int (+ main_i 1)) (nth main_t main_i))) (def main_j (+ main_i 1)) (while (> main_j 1) (do (def main_t (assoc main_t main_j (+ (nth main_t main_j) (nth main_t (- main_j 1))))) (def main_j (- main_j 1)))) (def main_cat (- (nth main_t (+ main_i 1)) (nth main_t main_i))) (if (< main_i 10) (println (str (str (str " " (str main_i)) " : ") (str main_cat))) (println (str (str (str main_i) " : ") (str main_cat)))))))

(-main)
