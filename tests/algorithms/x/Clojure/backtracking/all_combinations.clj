(ns main (:refer-clojure :exclude [create_all_state generate_all_combinations]))

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

(declare create_all_state generate_all_combinations)

(def ^:dynamic create_all_state_i nil)

(def ^:dynamic create_all_state_next_current nil)

(def ^:dynamic create_all_state_result nil)

(def ^:dynamic generate_all_combinations_result nil)

(defn create_all_state [create_all_state_increment create_all_state_total create_all_state_level create_all_state_current create_all_state_result_p]
  (binding [create_all_state_i nil create_all_state_next_current nil create_all_state_result nil] (try (do (set! create_all_state_result create_all_state_result_p) (when (= create_all_state_level 0) (throw (ex-info "return" {:v (conj create_all_state_result create_all_state_current)}))) (set! create_all_state_i create_all_state_increment) (while (<= create_all_state_i (+ (- create_all_state_total create_all_state_level) 1)) (do (set! create_all_state_next_current (conj create_all_state_current create_all_state_i)) (set! create_all_state_result (create_all_state (+ create_all_state_i 1) create_all_state_total (- create_all_state_level 1) create_all_state_next_current create_all_state_result)) (set! create_all_state_i (+ create_all_state_i 1)))) (throw (ex-info "return" {:v create_all_state_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn generate_all_combinations [generate_all_combinations_n generate_all_combinations_k]
  (binding [generate_all_combinations_result nil] (try (do (when (or (< generate_all_combinations_k 0) (< generate_all_combinations_n 0)) (throw (ex-info "return" {:v []}))) (set! generate_all_combinations_result []) (throw (ex-info "return" {:v (create_all_state 1 generate_all_combinations_n generate_all_combinations_k [] generate_all_combinations_result)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (generate_all_combinations 4 2)))
      (println (str (generate_all_combinations 3 1)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
