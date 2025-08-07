(ns main (:refer-clojure :exclude [int_pow backtrack solve]))

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

(declare int_pow backtrack solve)

(def ^:dynamic backtrack_p nil)

(def ^:dynamic count_v nil)

(def ^:dynamic int_pow_i nil)

(def ^:dynamic int_pow_result nil)

(defn int_pow [int_pow_base int_pow_exp]
  (binding [int_pow_i nil int_pow_result nil] (try (do (set! int_pow_result 1) (set! int_pow_i 0) (while (< int_pow_i int_pow_exp) (do (set! int_pow_result (* int_pow_result int_pow_base)) (set! int_pow_i (+ int_pow_i 1)))) (throw (ex-info "return" {:v int_pow_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn backtrack [backtrack_target backtrack_exp backtrack_current backtrack_current_sum]
  (binding [backtrack_p nil count_v nil] (try (do (when (= backtrack_current_sum backtrack_target) (throw (ex-info "return" {:v 1}))) (set! backtrack_p (int_pow backtrack_current backtrack_exp)) (set! count_v 0) (when (<= (+ backtrack_current_sum backtrack_p) backtrack_target) (set! count_v (+ count_v (backtrack backtrack_target backtrack_exp (+ backtrack_current 1) (+ backtrack_current_sum backtrack_p))))) (when (< backtrack_p backtrack_target) (set! count_v (+ count_v (backtrack backtrack_target backtrack_exp (+ backtrack_current 1) backtrack_current_sum)))) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solve [solve_target solve_exp]
  (try (do (when (not (and (and (and (<= 1 solve_target) (<= solve_target 1000)) (<= 2 solve_exp)) (<= solve_exp 10))) (do (println "Invalid input") (throw (ex-info "return" {:v 0})))) (throw (ex-info "return" {:v (backtrack solve_target solve_exp 1 0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (solve 13 2))
      (println (solve 10 2))
      (println (solve 10 3))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
