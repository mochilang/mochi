(ns main (:refer-clojure :exclude [cramers_rule_2x2 test_cramers_rule_2x2 main]))

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

(declare cramers_rule_2x2 test_cramers_rule_2x2 main)

(def ^:dynamic cramers_rule_2x2_a1 nil)

(def ^:dynamic cramers_rule_2x2_a2 nil)

(def ^:dynamic cramers_rule_2x2_b1 nil)

(def ^:dynamic cramers_rule_2x2_b2 nil)

(def ^:dynamic cramers_rule_2x2_c1 nil)

(def ^:dynamic cramers_rule_2x2_c2 nil)

(def ^:dynamic cramers_rule_2x2_determinant nil)

(def ^:dynamic cramers_rule_2x2_determinant_x nil)

(def ^:dynamic cramers_rule_2x2_determinant_y nil)

(def ^:dynamic cramers_rule_2x2_x nil)

(def ^:dynamic cramers_rule_2x2_y nil)

(def ^:dynamic test_cramers_rule_2x2_r1 nil)

(def ^:dynamic test_cramers_rule_2x2_r2 nil)

(defn cramers_rule_2x2 [cramers_rule_2x2_eq1 cramers_rule_2x2_eq2]
  (binding [cramers_rule_2x2_a1 nil cramers_rule_2x2_a2 nil cramers_rule_2x2_b1 nil cramers_rule_2x2_b2 nil cramers_rule_2x2_c1 nil cramers_rule_2x2_c2 nil cramers_rule_2x2_determinant nil cramers_rule_2x2_determinant_x nil cramers_rule_2x2_determinant_y nil cramers_rule_2x2_x nil cramers_rule_2x2_y nil] (try (do (when (or (not= (count cramers_rule_2x2_eq1) 3) (not= (count cramers_rule_2x2_eq2) 3)) (throw (Exception. "Please enter a valid equation."))) (when (and (and (and (= (nth cramers_rule_2x2_eq1 0) 0.0) (= (nth cramers_rule_2x2_eq1 1) 0.0)) (= (nth cramers_rule_2x2_eq2 0) 0.0)) (= (nth cramers_rule_2x2_eq2 1) 0.0)) (throw (Exception. "Both a & b of two equations can't be zero."))) (set! cramers_rule_2x2_a1 (nth cramers_rule_2x2_eq1 0)) (set! cramers_rule_2x2_b1 (nth cramers_rule_2x2_eq1 1)) (set! cramers_rule_2x2_c1 (nth cramers_rule_2x2_eq1 2)) (set! cramers_rule_2x2_a2 (nth cramers_rule_2x2_eq2 0)) (set! cramers_rule_2x2_b2 (nth cramers_rule_2x2_eq2 1)) (set! cramers_rule_2x2_c2 (nth cramers_rule_2x2_eq2 2)) (set! cramers_rule_2x2_determinant (- (* cramers_rule_2x2_a1 cramers_rule_2x2_b2) (* cramers_rule_2x2_a2 cramers_rule_2x2_b1))) (set! cramers_rule_2x2_determinant_x (- (* cramers_rule_2x2_c1 cramers_rule_2x2_b2) (* cramers_rule_2x2_c2 cramers_rule_2x2_b1))) (set! cramers_rule_2x2_determinant_y (- (* cramers_rule_2x2_a1 cramers_rule_2x2_c2) (* cramers_rule_2x2_a2 cramers_rule_2x2_c1))) (when (= cramers_rule_2x2_determinant 0.0) (do (when (and (= cramers_rule_2x2_determinant_x 0.0) (= cramers_rule_2x2_determinant_y 0.0)) (throw (Exception. "Infinite solutions. (Consistent system)"))) (throw (Exception. "No solution. (Inconsistent system)")))) (when (and (= cramers_rule_2x2_determinant_x 0.0) (= cramers_rule_2x2_determinant_y 0.0)) (throw (ex-info "return" {:v [0.0 0.0]}))) (set! cramers_rule_2x2_x (/ cramers_rule_2x2_determinant_x cramers_rule_2x2_determinant)) (set! cramers_rule_2x2_y (/ cramers_rule_2x2_determinant_y cramers_rule_2x2_determinant)) (throw (ex-info "return" {:v [cramers_rule_2x2_x cramers_rule_2x2_y]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_cramers_rule_2x2 []
  (binding [test_cramers_rule_2x2_r1 nil test_cramers_rule_2x2_r2 nil] (do (set! test_cramers_rule_2x2_r1 (cramers_rule_2x2 [2.0 3.0 0.0] [5.0 1.0 0.0])) (when (or (not= (nth test_cramers_rule_2x2_r1 0) 0.0) (not= (nth test_cramers_rule_2x2_r1 1) 0.0)) (throw (Exception. "Test1 failed"))) (set! test_cramers_rule_2x2_r2 (cramers_rule_2x2 [0.0 4.0 50.0] [2.0 0.0 26.0])) (when (or (not= (nth test_cramers_rule_2x2_r2 0) 13.0) (not= (nth test_cramers_rule_2x2_r2 1) 12.5)) (throw (Exception. "Test2 failed"))))))

(defn main []
  (do (test_cramers_rule_2x2) (println (cramers_rule_2x2 [11.0 2.0 30.0] [1.0 0.0 4.0]))))

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
