(ns main (:refer-clojure :exclude [sum_of_series test_sum_of_series main]))

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

(declare sum_of_series test_sum_of_series main)

(def ^:dynamic sum_of_series_total nil)

(defn sum_of_series [sum_of_series_first_term sum_of_series_common_diff sum_of_series_num_of_terms]
  (binding [sum_of_series_total nil] (try (do (set! sum_of_series_total (quot (* sum_of_series_num_of_terms (+ (* 2 sum_of_series_first_term) (* (- sum_of_series_num_of_terms 1) sum_of_series_common_diff))) 2)) (throw (ex-info "return" {:v sum_of_series_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_sum_of_series []
  (do (when (not= (sum_of_series 1 1 10) 55) (throw (Exception. "sum_of_series(1, 1, 10) failed"))) (when (not= (sum_of_series 1 10 100) 49600) (throw (Exception. "sum_of_series(1, 10, 100) failed")))))

(defn main []
  (do (test_sum_of_series) (println (sum_of_series 1 1 10))))

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
