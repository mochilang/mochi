(ns main (:refer-clojure :exclude [dp_count]))

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

(declare dp_count)

(def ^:dynamic dp_count_coin_val nil)

(def ^:dynamic dp_count_i nil)

(def ^:dynamic dp_count_idx nil)

(def ^:dynamic dp_count_j nil)

(def ^:dynamic dp_count_table nil)

(defn dp_count [dp_count_s dp_count_n]
  (binding [dp_count_coin_val nil dp_count_i nil dp_count_idx nil dp_count_j nil dp_count_table nil] (try (do (when (< dp_count_n 0) (throw (ex-info "return" {:v 0}))) (set! dp_count_table []) (set! dp_count_i 0) (while (<= dp_count_i dp_count_n) (do (set! dp_count_table (conj dp_count_table 0)) (set! dp_count_i (+ dp_count_i 1)))) (set! dp_count_table (assoc dp_count_table 0 1)) (set! dp_count_idx 0) (while (< dp_count_idx (count dp_count_s)) (do (set! dp_count_coin_val (nth dp_count_s dp_count_idx)) (set! dp_count_j dp_count_coin_val) (while (<= dp_count_j dp_count_n) (do (set! dp_count_table (assoc dp_count_table dp_count_j (+ (nth dp_count_table dp_count_j) (nth dp_count_table (- dp_count_j dp_count_coin_val))))) (set! dp_count_j (+ dp_count_j 1)))) (set! dp_count_idx (+ dp_count_idx 1)))) (throw (ex-info "return" {:v (nth dp_count_table dp_count_n)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (dp_count [1 2 3] 4))
      (println (dp_count [1 2 3] 7))
      (println (dp_count [2 5 3 6] 10))
      (println (dp_count [10] 99))
      (println (dp_count [4 5 6] 0))
      (println (dp_count [1 2 3] (- 5)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
