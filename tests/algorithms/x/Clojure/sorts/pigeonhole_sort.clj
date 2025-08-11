(ns main (:refer-clojure :exclude [pigeonhole_sort]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(defn toi [s]
  (Integer/parseInt (str s)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare pigeonhole_sort)

(def ^:dynamic count_v nil)

(def ^:dynamic main_j nil)

(def ^:dynamic main_output nil)

(def ^:dynamic pigeonhole_sort_arr nil)

(def ^:dynamic pigeonhole_sort_holes nil)

(def ^:dynamic pigeonhole_sort_i nil)

(def ^:dynamic pigeonhole_sort_index nil)

(def ^:dynamic pigeonhole_sort_max_val nil)

(def ^:dynamic pigeonhole_sort_min_val nil)

(def ^:dynamic pigeonhole_sort_size nil)

(def ^:dynamic pigeonhole_sort_sorted_index nil)

(def ^:dynamic pigeonhole_sort_x nil)

(defn pigeonhole_sort [pigeonhole_sort_arr_p]
  (binding [count_v nil pigeonhole_sort_arr nil pigeonhole_sort_holes nil pigeonhole_sort_i nil pigeonhole_sort_index nil pigeonhole_sort_max_val nil pigeonhole_sort_min_val nil pigeonhole_sort_size nil pigeonhole_sort_sorted_index nil pigeonhole_sort_x nil] (try (do (set! pigeonhole_sort_arr pigeonhole_sort_arr_p) (when (= (count pigeonhole_sort_arr) 0) (throw (ex-info "return" {:v pigeonhole_sort_arr}))) (set! pigeonhole_sort_min_val (long (apply min pigeonhole_sort_arr))) (set! pigeonhole_sort_max_val (long (apply max pigeonhole_sort_arr))) (set! pigeonhole_sort_size (+ (- pigeonhole_sort_max_val pigeonhole_sort_min_val) 1)) (set! pigeonhole_sort_holes []) (set! pigeonhole_sort_i 0) (while (< pigeonhole_sort_i pigeonhole_sort_size) (do (set! pigeonhole_sort_holes (conj pigeonhole_sort_holes 0)) (set! pigeonhole_sort_i (+ pigeonhole_sort_i 1)))) (set! pigeonhole_sort_i 0) (while (< pigeonhole_sort_i (count pigeonhole_sort_arr)) (do (set! pigeonhole_sort_x (nth pigeonhole_sort_arr pigeonhole_sort_i)) (set! pigeonhole_sort_index (- pigeonhole_sort_x pigeonhole_sort_min_val)) (set! pigeonhole_sort_holes (assoc pigeonhole_sort_holes pigeonhole_sort_index (+ (nth pigeonhole_sort_holes pigeonhole_sort_index) 1))) (set! pigeonhole_sort_i (+ pigeonhole_sort_i 1)))) (set! pigeonhole_sort_sorted_index 0) (set! count_v 0) (while (< count_v pigeonhole_sort_size) (do (while (> (nth pigeonhole_sort_holes count_v) 0) (do (set! pigeonhole_sort_arr (assoc pigeonhole_sort_arr pigeonhole_sort_sorted_index (+ count_v pigeonhole_sort_min_val))) (set! pigeonhole_sort_holes (assoc pigeonhole_sort_holes count_v (- (nth pigeonhole_sort_holes count_v) 1))) (set! pigeonhole_sort_sorted_index (+ pigeonhole_sort_sorted_index 1)))) (set! count_v (+ count_v 1)))) (throw (ex-info "return" {:v pigeonhole_sort_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_example [8 3 2 7 4 6 8])

(def ^:dynamic main_result (pigeonhole_sort main_example))

(def ^:dynamic main_output "Sorted order is:")

(def ^:dynamic main_j 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (< main_j (count main_result)) (do (def main_output (str (str main_output " ") (str (nth main_result main_j)))) (def main_j (+ main_j 1))))
      (println main_output)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
