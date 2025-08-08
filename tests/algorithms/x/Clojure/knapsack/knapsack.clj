(ns main (:refer-clojure :exclude [knapsack main]))

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

(declare knapsack main)

(def ^:dynamic count_v nil)

(def ^:dynamic knapsack_left_capacity nil)

(def ^:dynamic knapsack_new_value_included nil)

(def ^:dynamic knapsack_without_new_value nil)

(def ^:dynamic main_cap nil)

(def ^:dynamic main_result nil)

(def ^:dynamic main_values nil)

(def ^:dynamic main_weights nil)

(defn knapsack [knapsack_capacity knapsack_weights knapsack_values knapsack_counter]
  (binding [knapsack_left_capacity nil knapsack_new_value_included nil knapsack_without_new_value nil] (try (do (when (or (= knapsack_counter 0) (= knapsack_capacity 0)) (throw (ex-info "return" {:v 0}))) (if (> (nth knapsack_weights (- knapsack_counter 1)) knapsack_capacity) (throw (ex-info "return" {:v (knapsack knapsack_capacity knapsack_weights knapsack_values (- knapsack_counter 1))})) (do (set! knapsack_left_capacity (- knapsack_capacity (nth knapsack_weights (- knapsack_counter 1)))) (set! knapsack_new_value_included (+ (nth knapsack_values (- knapsack_counter 1)) (knapsack knapsack_left_capacity knapsack_weights knapsack_values (- knapsack_counter 1)))) (set! knapsack_without_new_value (knapsack knapsack_capacity knapsack_weights knapsack_values (- knapsack_counter 1))) (if (> knapsack_new_value_included knapsack_without_new_value) (throw (ex-info "return" {:v knapsack_new_value_included})) (throw (ex-info "return" {:v knapsack_without_new_value})))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [count_v nil main_cap nil main_result nil main_values nil main_weights nil] (do (set! main_weights [10 20 30]) (set! main_values [60 100 120]) (set! main_cap 50) (set! count_v (count main_values)) (set! main_result (knapsack main_cap main_weights main_values count_v)) (println (str main_result)))))

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
