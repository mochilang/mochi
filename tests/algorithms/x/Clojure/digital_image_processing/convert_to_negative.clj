(ns main (:refer-clojure :exclude [convert_to_negative main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare convert_to_negative main)

(def ^:dynamic convert_to_negative_b nil)

(def ^:dynamic convert_to_negative_g nil)

(def ^:dynamic convert_to_negative_i nil)

(def ^:dynamic convert_to_negative_j nil)

(def ^:dynamic convert_to_negative_pixel nil)

(def ^:dynamic convert_to_negative_r nil)

(def ^:dynamic convert_to_negative_result nil)

(def ^:dynamic convert_to_negative_row nil)

(def ^:dynamic main_image nil)

(def ^:dynamic main_neg nil)

(defn convert_to_negative [convert_to_negative_img]
  (binding [convert_to_negative_b nil convert_to_negative_g nil convert_to_negative_i nil convert_to_negative_j nil convert_to_negative_pixel nil convert_to_negative_r nil convert_to_negative_result nil convert_to_negative_row nil] (try (do (set! convert_to_negative_result []) (set! convert_to_negative_i 0) (while (< convert_to_negative_i (count convert_to_negative_img)) (do (set! convert_to_negative_row []) (set! convert_to_negative_j 0) (while (< convert_to_negative_j (count (nth convert_to_negative_img convert_to_negative_i))) (do (set! convert_to_negative_pixel (nth (nth convert_to_negative_img convert_to_negative_i) convert_to_negative_j)) (set! convert_to_negative_r (- 255 (nth convert_to_negative_pixel 0))) (set! convert_to_negative_g (- 255 (nth convert_to_negative_pixel 1))) (set! convert_to_negative_b (- 255 (nth convert_to_negative_pixel 2))) (set! convert_to_negative_row (conj convert_to_negative_row [convert_to_negative_r convert_to_negative_g convert_to_negative_b])) (set! convert_to_negative_j (+ convert_to_negative_j 1)))) (set! convert_to_negative_result (conj convert_to_negative_result convert_to_negative_row)) (set! convert_to_negative_i (+ convert_to_negative_i 1)))) (throw (ex-info "return" {:v convert_to_negative_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_image nil main_neg nil] (do (set! main_image [[[10 20 30] [0 0 0]] [[255 255 255] [100 150 200]]]) (set! main_neg (convert_to_negative main_image)) (println main_neg))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (main)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
