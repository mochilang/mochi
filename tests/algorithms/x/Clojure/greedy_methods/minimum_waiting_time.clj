(ns main (:refer-clojure :exclude [insertion_sort minimum_waiting_time]))

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

(declare insertion_sort minimum_waiting_time)

(def ^:dynamic insertion_sort_a nil)

(def ^:dynamic insertion_sort_i nil)

(def ^:dynamic insertion_sort_j nil)

(def ^:dynamic insertion_sort_key nil)

(def ^:dynamic minimum_waiting_time_i nil)

(def ^:dynamic minimum_waiting_time_n nil)

(def ^:dynamic minimum_waiting_time_sorted nil)

(def ^:dynamic minimum_waiting_time_total nil)

(defn insertion_sort [insertion_sort_a_p]
  (binding [insertion_sort_a nil insertion_sort_i nil insertion_sort_j nil insertion_sort_key nil] (try (do (set! insertion_sort_a insertion_sort_a_p) (set! insertion_sort_i 1) (while (< insertion_sort_i (count insertion_sort_a)) (do (set! insertion_sort_key (nth insertion_sort_a insertion_sort_i)) (set! insertion_sort_j (- insertion_sort_i 1)) (while (and (>= insertion_sort_j 0) (> (nth insertion_sort_a insertion_sort_j) insertion_sort_key)) (do (set! insertion_sort_a (assoc insertion_sort_a (+ insertion_sort_j 1) (nth insertion_sort_a insertion_sort_j))) (set! insertion_sort_j (- insertion_sort_j 1)))) (set! insertion_sort_a (assoc insertion_sort_a (+ insertion_sort_j 1) insertion_sort_key)) (set! insertion_sort_i (+ insertion_sort_i 1)))) (throw (ex-info "return" {:v insertion_sort_a}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn minimum_waiting_time [minimum_waiting_time_queries]
  (binding [minimum_waiting_time_i nil minimum_waiting_time_n nil minimum_waiting_time_sorted nil minimum_waiting_time_total nil] (try (do (set! minimum_waiting_time_n (count minimum_waiting_time_queries)) (when (or (= minimum_waiting_time_n 0) (= minimum_waiting_time_n 1)) (throw (ex-info "return" {:v 0}))) (set! minimum_waiting_time_sorted (insertion_sort minimum_waiting_time_queries)) (set! minimum_waiting_time_total 0) (set! minimum_waiting_time_i 0) (while (< minimum_waiting_time_i minimum_waiting_time_n) (do (set! minimum_waiting_time_total (+ minimum_waiting_time_total (* (nth minimum_waiting_time_sorted minimum_waiting_time_i) (- (- minimum_waiting_time_n minimum_waiting_time_i) 1)))) (set! minimum_waiting_time_i (+ minimum_waiting_time_i 1)))) (throw (ex-info "return" {:v minimum_waiting_time_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (minimum_waiting_time [3 2 1 2 6]))
      (println (minimum_waiting_time [3 2 1]))
      (println (minimum_waiting_time [1 2 3 4]))
      (println (minimum_waiting_time [5 5 5 5]))
      (println (minimum_waiting_time []))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
