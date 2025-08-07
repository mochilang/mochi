(ns main (:refer-clojure :exclude [mean_threshold print_image]))

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

(declare mean_threshold print_image)

(def ^:dynamic mean_threshold_height nil)

(def ^:dynamic mean_threshold_i nil)

(def ^:dynamic mean_threshold_image nil)

(def ^:dynamic mean_threshold_j nil)

(def ^:dynamic mean_threshold_mean nil)

(def ^:dynamic mean_threshold_total nil)

(def ^:dynamic mean_threshold_width nil)

(def ^:dynamic print_image_i nil)

(defn mean_threshold [mean_threshold_image_p]
  (binding [mean_threshold_height nil mean_threshold_i nil mean_threshold_image nil mean_threshold_j nil mean_threshold_mean nil mean_threshold_total nil mean_threshold_width nil] (try (do (set! mean_threshold_image mean_threshold_image_p) (set! mean_threshold_height (count mean_threshold_image)) (set! mean_threshold_width (count (nth mean_threshold_image 0))) (set! mean_threshold_total 0) (set! mean_threshold_i 0) (while (< mean_threshold_i mean_threshold_height) (do (set! mean_threshold_j 0) (while (< mean_threshold_j mean_threshold_width) (do (set! mean_threshold_total (+ mean_threshold_total (nth (nth mean_threshold_image mean_threshold_i) mean_threshold_j))) (set! mean_threshold_j (+ mean_threshold_j 1)))) (set! mean_threshold_i (+ mean_threshold_i 1)))) (set! mean_threshold_mean (/ mean_threshold_total (* mean_threshold_height mean_threshold_width))) (set! mean_threshold_i 0) (while (< mean_threshold_i mean_threshold_height) (do (set! mean_threshold_j 0) (while (< mean_threshold_j mean_threshold_width) (do (if (> (nth (nth mean_threshold_image mean_threshold_i) mean_threshold_j) mean_threshold_mean) (set! mean_threshold_image (assoc-in mean_threshold_image [mean_threshold_i mean_threshold_j] 255)) (set! mean_threshold_image (assoc-in mean_threshold_image [mean_threshold_i mean_threshold_j] 0))) (set! mean_threshold_j (+ mean_threshold_j 1)))) (set! mean_threshold_i (+ mean_threshold_i 1)))) (throw (ex-info "return" {:v mean_threshold_image}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_image [print_image_image]
  (binding [print_image_i nil] (do (set! print_image_i 0) (while (< print_image_i (count print_image_image)) (do (println (nth print_image_image print_image_i)) (set! print_image_i (+ print_image_i 1)))))))

(def ^:dynamic main_img [[10 200 50] [100 150 30] [90 80 220]])

(def ^:dynamic main_result (mean_threshold main_img))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (print_image main_result)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
