(ns main (:refer-clojure :exclude [segment_image main]))

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

(declare segment_image main)

(def ^:dynamic main_image nil)

(def ^:dynamic main_segmented nil)

(def ^:dynamic main_thresholds nil)

(def ^:dynamic segment_image_i nil)

(def ^:dynamic segment_image_j nil)

(def ^:dynamic segment_image_k nil)

(def ^:dynamic segment_image_label nil)

(def ^:dynamic segment_image_pixel nil)

(def ^:dynamic segment_image_row nil)

(def ^:dynamic segment_image_segmented nil)

(defn segment_image [segment_image_image segment_image_thresholds]
  (binding [segment_image_i nil segment_image_j nil segment_image_k nil segment_image_label nil segment_image_pixel nil segment_image_row nil segment_image_segmented nil] (try (do (set! segment_image_segmented []) (set! segment_image_i 0) (while (< segment_image_i (count segment_image_image)) (do (set! segment_image_row []) (set! segment_image_j 0) (while (< segment_image_j (count (nth segment_image_image segment_image_i))) (do (set! segment_image_pixel (nth (nth segment_image_image segment_image_i) segment_image_j)) (set! segment_image_label 0) (set! segment_image_k 0) (while (< segment_image_k (count segment_image_thresholds)) (do (when (> segment_image_pixel (nth segment_image_thresholds segment_image_k)) (set! segment_image_label (+ segment_image_k 1))) (set! segment_image_k (+ segment_image_k 1)))) (set! segment_image_row (conj segment_image_row segment_image_label)) (set! segment_image_j (+ segment_image_j 1)))) (set! segment_image_segmented (conj segment_image_segmented segment_image_row)) (set! segment_image_i (+ segment_image_i 1)))) (throw (ex-info "return" {:v segment_image_segmented}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_image nil main_segmented nil main_thresholds nil] (do (set! main_image [[80 120 180] [40 90 150] [20 60 100]]) (set! main_thresholds [50 100 150]) (set! main_segmented (segment_image main_image main_thresholds)) (println main_segmented))))

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
