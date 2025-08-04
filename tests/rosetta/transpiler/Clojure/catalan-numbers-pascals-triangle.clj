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
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (dotimes [_ (+ main_n 2)] (def main_t (conj main_t 0)))
      (def main_t (assoc main_t 1 1))
      (doseq [i (range 1 (+ main_n 1))] (do (def main_j i) (while (> main_j 1) (do (def main_t (assoc main_t main_j (+ (nth main_t main_j) (nth main_t (- main_j 1))))) (def main_j (- main_j 1)))) (def main_t (assoc main_t (long (+ i 1)) (nth main_t i))) (def main_j (+ i 1)) (while (> main_j 1) (do (def main_t (assoc main_t main_j (+ (nth main_t main_j) (nth main_t (- main_j 1))))) (def main_j (- main_j 1)))) (def main_cat (- (nth main_t (+ i 1)) (nth main_t i))) (if (< i 10) (println (str (str (str " " (str i)) " : ") (str main_cat))) (println (str (str (str i) " : ") (str main_cat))))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
