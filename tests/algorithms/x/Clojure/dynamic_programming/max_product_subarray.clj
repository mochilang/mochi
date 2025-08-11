(ns main (:refer-clojure :exclude [max_product_subarray]))

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

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare max_product_subarray)

(def ^:dynamic max_product_subarray_i nil)

(def ^:dynamic max_product_subarray_max_prod nil)

(def ^:dynamic max_product_subarray_max_till_now nil)

(def ^:dynamic max_product_subarray_min_till_now nil)

(def ^:dynamic max_product_subarray_number nil)

(def ^:dynamic max_product_subarray_prod_max nil)

(def ^:dynamic max_product_subarray_prod_min nil)

(def ^:dynamic max_product_subarray_temp nil)

(defn max_product_subarray [max_product_subarray_numbers]
  (binding [max_product_subarray_i nil max_product_subarray_max_prod nil max_product_subarray_max_till_now nil max_product_subarray_min_till_now nil max_product_subarray_number nil max_product_subarray_prod_max nil max_product_subarray_prod_min nil max_product_subarray_temp nil] (try (do (when (= (count max_product_subarray_numbers) 0) (throw (ex-info "return" {:v 0}))) (set! max_product_subarray_max_till_now (nth max_product_subarray_numbers 0)) (set! max_product_subarray_min_till_now (nth max_product_subarray_numbers 0)) (set! max_product_subarray_max_prod (nth max_product_subarray_numbers 0)) (set! max_product_subarray_i 1) (while (< max_product_subarray_i (count max_product_subarray_numbers)) (do (set! max_product_subarray_number (nth max_product_subarray_numbers max_product_subarray_i)) (when (< max_product_subarray_number 0) (do (set! max_product_subarray_temp max_product_subarray_max_till_now) (set! max_product_subarray_max_till_now max_product_subarray_min_till_now) (set! max_product_subarray_min_till_now max_product_subarray_temp))) (set! max_product_subarray_prod_max (* max_product_subarray_max_till_now max_product_subarray_number)) (if (> max_product_subarray_number max_product_subarray_prod_max) (set! max_product_subarray_max_till_now max_product_subarray_number) (set! max_product_subarray_max_till_now max_product_subarray_prod_max)) (set! max_product_subarray_prod_min (* max_product_subarray_min_till_now max_product_subarray_number)) (if (< max_product_subarray_number max_product_subarray_prod_min) (set! max_product_subarray_min_till_now max_product_subarray_number) (set! max_product_subarray_min_till_now max_product_subarray_prod_min)) (when (> max_product_subarray_max_till_now max_product_subarray_max_prod) (set! max_product_subarray_max_prod max_product_subarray_max_till_now)) (set! max_product_subarray_i (+ max_product_subarray_i 1)))) (throw (ex-info "return" {:v max_product_subarray_max_prod}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (max_product_subarray [2 3 (- 2) 4]))
      (println (max_product_subarray [(- 2) 0 (- 1)]))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
