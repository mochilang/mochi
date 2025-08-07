(ns main (:refer-clojure :exclude [make_list histogram_stretch print_image]))

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

(declare make_list histogram_stretch print_image)

(def ^:dynamic histogram_stretch_cumulative nil)

(def ^:dynamic histogram_stretch_h nil)

(def ^:dynamic histogram_stretch_height nil)

(def ^:dynamic histogram_stretch_hist nil)

(def ^:dynamic histogram_stretch_i nil)

(def ^:dynamic histogram_stretch_image nil)

(def ^:dynamic histogram_stretch_j nil)

(def ^:dynamic histogram_stretch_mapping nil)

(def ^:dynamic histogram_stretch_total nil)

(def ^:dynamic histogram_stretch_val nil)

(def ^:dynamic histogram_stretch_width nil)

(def ^:dynamic make_list_i nil)

(def ^:dynamic make_list_res nil)

(def ^:dynamic print_image_i nil)

(defn make_list [make_list_n make_list_value]
  (binding [make_list_i nil make_list_res nil] (try (do (set! make_list_res []) (set! make_list_i 0) (while (< make_list_i make_list_n) (do (set! make_list_res (conj make_list_res make_list_value)) (set! make_list_i (+ make_list_i 1)))) (throw (ex-info "return" {:v make_list_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn histogram_stretch [histogram_stretch_image_p]
  (binding [histogram_stretch_cumulative nil histogram_stretch_h nil histogram_stretch_height nil histogram_stretch_hist nil histogram_stretch_i nil histogram_stretch_image nil histogram_stretch_j nil histogram_stretch_mapping nil histogram_stretch_total nil histogram_stretch_val nil histogram_stretch_width nil] (try (do (set! histogram_stretch_image histogram_stretch_image_p) (set! histogram_stretch_height (count histogram_stretch_image)) (set! histogram_stretch_width (count (nth histogram_stretch_image 0))) (set! histogram_stretch_hist (make_list 256 0)) (set! histogram_stretch_i 0) (while (< histogram_stretch_i histogram_stretch_height) (do (set! histogram_stretch_j 0) (while (< histogram_stretch_j histogram_stretch_width) (do (set! histogram_stretch_val (nth (nth histogram_stretch_image histogram_stretch_i) histogram_stretch_j)) (set! histogram_stretch_hist (assoc histogram_stretch_hist histogram_stretch_val (+ (nth histogram_stretch_hist histogram_stretch_val) 1))) (set! histogram_stretch_j (+ histogram_stretch_j 1)))) (set! histogram_stretch_i (+ histogram_stretch_i 1)))) (set! histogram_stretch_mapping (make_list 256 0)) (set! histogram_stretch_cumulative 0) (set! histogram_stretch_total (* histogram_stretch_height histogram_stretch_width)) (set! histogram_stretch_h 0) (while (< histogram_stretch_h 256) (do (set! histogram_stretch_cumulative (+ histogram_stretch_cumulative (nth histogram_stretch_hist histogram_stretch_h))) (set! histogram_stretch_mapping (assoc histogram_stretch_mapping histogram_stretch_h (quot (* 255 histogram_stretch_cumulative) histogram_stretch_total))) (set! histogram_stretch_h (+ histogram_stretch_h 1)))) (set! histogram_stretch_i 0) (while (< histogram_stretch_i histogram_stretch_height) (do (set! histogram_stretch_j 0) (while (< histogram_stretch_j histogram_stretch_width) (do (set! histogram_stretch_val (nth (nth histogram_stretch_image histogram_stretch_i) histogram_stretch_j)) (set! histogram_stretch_image (assoc-in histogram_stretch_image [histogram_stretch_i histogram_stretch_j] (nth histogram_stretch_mapping histogram_stretch_val))) (set! histogram_stretch_j (+ histogram_stretch_j 1)))) (set! histogram_stretch_i (+ histogram_stretch_i 1)))) (throw (ex-info "return" {:v histogram_stretch_image}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_image [print_image_image]
  (binding [print_image_i nil] (do (set! print_image_i 0) (while (< print_image_i (count print_image_image)) (do (println (nth print_image_image print_image_i)) (set! print_image_i (+ print_image_i 1)))))))

(def ^:dynamic main_img [[52 55 61] [59 79 61] [85 76 62]])

(def ^:dynamic main_result (histogram_stretch main_img))

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
