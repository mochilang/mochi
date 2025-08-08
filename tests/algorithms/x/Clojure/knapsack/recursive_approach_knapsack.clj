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

(def ^:dynamic knapsack_ans1 nil)

(def ^:dynamic knapsack_ans2 nil)

(def ^:dynamic main_r1 nil)

(def ^:dynamic main_r2 nil)

(def ^:dynamic main_v1 nil)

(def ^:dynamic main_v2 nil)

(def ^:dynamic main_w1 nil)

(def ^:dynamic main_w2 nil)

(defn knapsack [knapsack_weights knapsack_values knapsack_number_of_items knapsack_max_weight knapsack_index]
  (binding [knapsack_ans1 nil knapsack_ans2 nil] (try (do (when (= knapsack_index knapsack_number_of_items) (throw (ex-info "return" {:v 0}))) (set! knapsack_ans1 (knapsack knapsack_weights knapsack_values knapsack_number_of_items knapsack_max_weight (+ knapsack_index 1))) (set! knapsack_ans2 0) (when (<= (nth knapsack_weights knapsack_index) knapsack_max_weight) (set! knapsack_ans2 (+ (nth knapsack_values knapsack_index) (knapsack knapsack_weights knapsack_values knapsack_number_of_items (- knapsack_max_weight (nth knapsack_weights knapsack_index)) (+ knapsack_index 1))))) (if (> knapsack_ans1 knapsack_ans2) knapsack_ans1 knapsack_ans2)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_r1 nil main_r2 nil main_v1 nil main_v2 nil main_w1 nil main_w2 nil] (do (set! main_w1 [1 2 4 5]) (set! main_v1 [5 4 8 6]) (set! main_r1 (knapsack main_w1 main_v1 4 5 0)) (println (str main_r1)) (set! main_w2 [3 4 5]) (set! main_v2 [10 9 8]) (set! main_r2 (knapsack main_w2 main_v2 3 25 0)) (println (str main_r2)))))

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
