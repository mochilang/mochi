(ns main (:refer-clojure :exclude [product_sum product_sum_array]))

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

(declare product_sum product_sum_array)

(def ^:dynamic product_sum_array_res nil)

(def ^:dynamic product_sum_el nil)

(def ^:dynamic product_sum_i nil)

(def ^:dynamic product_sum_total nil)

(defn product_sum [product_sum_arr product_sum_depth]
  (binding [product_sum_el nil product_sum_i nil product_sum_total nil] (try (do (set! product_sum_total 0) (set! product_sum_i 0) (while (< product_sum_i (count product_sum_arr)) (do (set! product_sum_el (nth product_sum_arr product_sum_i)) (if (> (count product_sum_el) 0) (set! product_sum_total (+ product_sum_total (product_sum product_sum_el (+ product_sum_depth 1)))) (set! product_sum_total (+ product_sum_total (long product_sum_el)))) (set! product_sum_i (+ product_sum_i 1)))) (throw (ex-info "return" {:v (* product_sum_total product_sum_depth)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn product_sum_array [product_sum_array_array]
  (binding [product_sum_array_res nil] (try (do (set! product_sum_array_res (product_sum product_sum_array_array 1)) (throw (ex-info "return" {:v product_sum_array_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_example [5 2 [(- 7) 1] 3 [6 [(- 13) 8] 4]])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (product_sum_array main_example))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
