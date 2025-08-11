(ns main (:refer-clojure :exclude [heapify heap_sort]))

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

(declare heapify heap_sort)

(def ^:dynamic heap_sort_arr nil)

(def ^:dynamic heap_sort_i nil)

(def ^:dynamic heap_sort_n nil)

(def ^:dynamic heap_sort_temp nil)

(def ^:dynamic heapify_arr nil)

(def ^:dynamic heapify_largest nil)

(def ^:dynamic heapify_left_index nil)

(def ^:dynamic heapify_right_index nil)

(def ^:dynamic heapify_temp nil)

(defn heapify [heapify_arr_p heapify_index heapify_heap_size]
  (binding [heapify_arr nil heapify_largest nil heapify_left_index nil heapify_right_index nil heapify_temp nil] (do (set! heapify_arr heapify_arr_p) (set! heapify_largest heapify_index) (set! heapify_left_index (+ (* 2 heapify_index) 1)) (set! heapify_right_index (+ (* 2 heapify_index) 2)) (when (and (< heapify_left_index heapify_heap_size) (> (nth heapify_arr heapify_left_index) (nth heapify_arr heapify_largest))) (set! heapify_largest heapify_left_index)) (when (and (< heapify_right_index heapify_heap_size) (> (nth heapify_arr heapify_right_index) (nth heapify_arr heapify_largest))) (set! heapify_largest heapify_right_index)) (when (not= heapify_largest heapify_index) (do (set! heapify_temp (nth heapify_arr heapify_largest)) (set! heapify_arr (assoc heapify_arr heapify_largest (nth heapify_arr heapify_index))) (set! heapify_arr (assoc heapify_arr heapify_index heapify_temp)) (heapify heapify_arr heapify_largest heapify_heap_size))) heapify_arr)))

(defn heap_sort [heap_sort_arr_p]
  (binding [heap_sort_arr nil heap_sort_i nil heap_sort_n nil heap_sort_temp nil] (try (do (set! heap_sort_arr heap_sort_arr_p) (set! heap_sort_n (count heap_sort_arr)) (set! heap_sort_i (- (/ heap_sort_n 2) 1)) (while (>= heap_sort_i 0) (do (heapify heap_sort_arr heap_sort_i heap_sort_n) (set! heap_sort_i (- heap_sort_i 1)))) (set! heap_sort_i (- heap_sort_n 1)) (while (> heap_sort_i 0) (do (set! heap_sort_temp (nth heap_sort_arr 0)) (set! heap_sort_arr (assoc heap_sort_arr 0 (nth heap_sort_arr heap_sort_i))) (set! heap_sort_arr (assoc heap_sort_arr heap_sort_i heap_sort_temp)) (heapify heap_sort_arr 0 heap_sort_i) (set! heap_sort_i (- heap_sort_i 1)))) (throw (ex-info "return" {:v heap_sort_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_data [3 7 9 28 123 (- 5) 8 (- 30) (- 200) 0 4])

(def ^:dynamic main_result (heap_sort main_data))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println main_result)
      (when (not= (str main_result) (str [(- 200) (- 30) (- 5) 0 3 4 7 8 9 28 123])) (throw (Exception. "Assertion error")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
