(ns main (:refer-clojure :exclude [get_failure_array knuth_morris_pratt]))

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

(declare get_failure_array knuth_morris_pratt)

(def ^:dynamic get_failure_array_failure nil)

(def ^:dynamic get_failure_array_i nil)

(def ^:dynamic get_failure_array_j nil)

(def ^:dynamic knuth_morris_pratt_failure nil)

(def ^:dynamic knuth_morris_pratt_i nil)

(def ^:dynamic knuth_morris_pratt_j nil)

(defn get_failure_array [get_failure_array_pattern]
  (binding [get_failure_array_failure nil get_failure_array_i nil get_failure_array_j nil] (try (do (set! get_failure_array_failure [0]) (set! get_failure_array_i 0) (set! get_failure_array_j 1) (loop [while_flag_1 true] (when (and while_flag_1 (< get_failure_array_j (count get_failure_array_pattern))) (do (if (= (subs get_failure_array_pattern get_failure_array_i (min (+ get_failure_array_i 1) (count get_failure_array_pattern))) (subs get_failure_array_pattern get_failure_array_j (min (+ get_failure_array_j 1) (count get_failure_array_pattern)))) (set! get_failure_array_i (+ get_failure_array_i 1)) (when (> get_failure_array_i 0) (do (set! get_failure_array_i (nth get_failure_array_failure (- get_failure_array_i 1))) (recur true)))) (set! get_failure_array_j (+ get_failure_array_j 1)) (set! get_failure_array_failure (conj get_failure_array_failure get_failure_array_i)) (cond :else (recur while_flag_1))))) (throw (ex-info "return" {:v get_failure_array_failure}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn knuth_morris_pratt [knuth_morris_pratt_text knuth_morris_pratt_pattern]
  (binding [knuth_morris_pratt_failure nil knuth_morris_pratt_i nil knuth_morris_pratt_j nil] (try (do (set! knuth_morris_pratt_failure (get_failure_array knuth_morris_pratt_pattern)) (set! knuth_morris_pratt_i 0) (set! knuth_morris_pratt_j 0) (loop [while_flag_2 true] (when (and while_flag_2 (< knuth_morris_pratt_i (count knuth_morris_pratt_text))) (do (if (= (subs knuth_morris_pratt_pattern knuth_morris_pratt_j (min (+ knuth_morris_pratt_j 1) (count knuth_morris_pratt_pattern))) (subs knuth_morris_pratt_text knuth_morris_pratt_i (min (+ knuth_morris_pratt_i 1) (count knuth_morris_pratt_text)))) (do (when (= knuth_morris_pratt_j (- (count knuth_morris_pratt_pattern) 1)) (throw (ex-info "return" {:v (- knuth_morris_pratt_i knuth_morris_pratt_j)}))) (set! knuth_morris_pratt_j (+ knuth_morris_pratt_j 1))) (when (> knuth_morris_pratt_j 0) (do (set! knuth_morris_pratt_j (nth knuth_morris_pratt_failure (- knuth_morris_pratt_j 1))) (recur true)))) (set! knuth_morris_pratt_i (+ knuth_morris_pratt_i 1)) (cond :else (recur while_flag_2))))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_text "abcxabcdabxabcdabcdabcy")

(def ^:dynamic main_pattern "abcdabcy")

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (knuth_morris_pratt main_text main_pattern))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
