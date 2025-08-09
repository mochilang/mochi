(ns main (:refer-clojure :exclude [knapsack test_base_case test_easy_case test_knapsack]))

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

(declare knapsack test_base_case test_easy_case test_knapsack)

(def ^:dynamic knapsack_exclude_val nil)

(def ^:dynamic knapsack_include_val nil)

(def ^:dynamic knapsack_left_capacity nil)

(def ^:dynamic test_base_case_c nil)

(def ^:dynamic test_base_case_c2 nil)

(def ^:dynamic test_base_case_cap nil)

(def ^:dynamic test_base_case_val nil)

(def ^:dynamic test_base_case_val2 nil)

(def ^:dynamic test_base_case_w nil)

(def ^:dynamic test_base_case_w2 nil)

(def ^:dynamic test_easy_case_c nil)

(def ^:dynamic test_easy_case_cap nil)

(def ^:dynamic test_easy_case_val nil)

(def ^:dynamic test_easy_case_w nil)

(def ^:dynamic test_knapsack_c nil)

(def ^:dynamic test_knapsack_cap nil)

(def ^:dynamic test_knapsack_val nil)

(def ^:dynamic test_knapsack_w nil)

(defn knapsack [knapsack_capacity knapsack_weights knapsack_values knapsack_counter]
  (binding [knapsack_exclude_val nil knapsack_include_val nil knapsack_left_capacity nil] (try (do (when (or (= knapsack_counter 0) (= knapsack_capacity 0)) (throw (ex-info "return" {:v 0}))) (when (> (nth knapsack_weights (- knapsack_counter 1)) knapsack_capacity) (throw (ex-info "return" {:v (knapsack knapsack_capacity knapsack_weights knapsack_values (- knapsack_counter 1))}))) (set! knapsack_left_capacity (- knapsack_capacity (nth knapsack_weights (- knapsack_counter 1)))) (set! knapsack_include_val (+ (nth knapsack_values (- knapsack_counter 1)) (knapsack knapsack_left_capacity knapsack_weights knapsack_values (- knapsack_counter 1)))) (set! knapsack_exclude_val (knapsack knapsack_capacity knapsack_weights knapsack_values (- knapsack_counter 1))) (if (> knapsack_include_val knapsack_exclude_val) knapsack_include_val knapsack_exclude_val)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_base_case []
  (binding [test_base_case_c nil test_base_case_c2 nil test_base_case_cap nil test_base_case_val nil test_base_case_val2 nil test_base_case_w nil test_base_case_w2 nil] (try (do (set! test_base_case_cap 0) (set! test_base_case_val [0]) (set! test_base_case_w [0]) (set! test_base_case_c (count test_base_case_val)) (when (not= (knapsack test_base_case_cap test_base_case_w test_base_case_val test_base_case_c) 0) (throw (ex-info "return" {:v false}))) (set! test_base_case_val2 [60]) (set! test_base_case_w2 [10]) (set! test_base_case_c2 (count test_base_case_val2)) (throw (ex-info "return" {:v (= (knapsack test_base_case_cap test_base_case_w2 test_base_case_val2 test_base_case_c2) 0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_easy_case []
  (binding [test_easy_case_c nil test_easy_case_cap nil test_easy_case_val nil test_easy_case_w nil] (try (do (set! test_easy_case_cap 3) (set! test_easy_case_val [1 2 3]) (set! test_easy_case_w [3 2 1]) (set! test_easy_case_c (count test_easy_case_val)) (throw (ex-info "return" {:v (= (knapsack test_easy_case_cap test_easy_case_w test_easy_case_val test_easy_case_c) 5)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_knapsack []
  (binding [test_knapsack_c nil test_knapsack_cap nil test_knapsack_val nil test_knapsack_w nil] (try (do (set! test_knapsack_cap 50) (set! test_knapsack_val [60 100 120]) (set! test_knapsack_w [10 20 30]) (set! test_knapsack_c (count test_knapsack_val)) (throw (ex-info "return" {:v (= (knapsack test_knapsack_cap test_knapsack_w test_knapsack_val test_knapsack_c) 220)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (test_base_case))
      (println (test_easy_case))
      (println (test_knapsack))
      (println true)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
