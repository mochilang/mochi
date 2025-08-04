(ns main)

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(def ^:dynamic main_c nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_j nil)

(def ^:dynamic main_line nil)

(def ^:dynamic main_n nil)

(def ^:dynamic main_r nil)

(def ^:dynamic main_row nil)

(def ^:dynamic main_rows nil)

(def ^:dynamic main_seen nil)

(def ^:dynamic main_u nil)

(def ^:dynamic main_nPts 100)

(def ^:dynamic main_rMin 10)

(def ^:dynamic main_rMax 15)

(def ^:dynamic main_span (+ (+ main_rMax 1) main_rMax))

(def ^:dynamic main_rows [])

(def ^:dynamic main_r 0)

(def ^:dynamic main_u 0)

(def ^:dynamic main_seen {})

(def ^:dynamic main_min2 (* main_rMin main_rMin))

(def ^:dynamic main_max2 (* main_rMax main_rMax))

(def ^:dynamic main_n 0)

(def ^:dynamic main_i 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (< main_r main_span) (do (def ^:dynamic main_row []) (def ^:dynamic main_c 0) (while (< main_c (* main_span 2)) (do (def main_row (conj main_row " ")) (def main_c (+ main_c 1)))) (def main_rows (conj main_rows main_row)) (def main_r (+ main_r 1))))
      (loop [while_flag_1 true] (when (and while_flag_1 (< main_n main_nPts)) (do (def ^:dynamic main_x (- (mod (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647))) main_span) main_rMax)) (def ^:dynamic main_y (- (mod (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647))) main_span) main_rMax)) (def ^:dynamic main_rs (+ (* main_x main_x) (* main_y main_y))) (cond (or (< main_rs main_min2) (> main_rs main_max2)) (recur true) :else (do (def main_n (+ main_n 1)) (def ^:dynamic main_row (+ main_y main_rMax)) (def ^:dynamic main_col (* (+ main_x main_rMax) 2)) (def main_rows (assoc-in main_rows [main_row main_col] "*")) (def ^:dynamic main_key (str (str (str main_row) ",") (str main_col))) (when (not (get main_seen main_key)) (do (def main_seen (assoc main_seen main_key true)) (def main_u (+ main_u 1)))) (recur while_flag_1))))))
      (while (< main_i main_span) (do (def ^:dynamic main_line "") (def ^:dynamic main_j 0) (while (< main_j (* main_span 2)) (do (def main_line (str main_line (nth (nth main_rows main_i) main_j))) (def main_j (+ main_j 1)))) (println main_line) (def main_i (+ main_i 1))))
      (println (str (str main_u) " unique points"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
