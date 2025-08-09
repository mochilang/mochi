(ns main (:refer-clojure :exclude [pivot findPivot interpret simplex]))

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

(declare pivot findPivot interpret simplex)

(def ^:dynamic findPivot_coeff nil)

(def ^:dynamic findPivot_col nil)

(def ^:dynamic findPivot_minRatio nil)

(def ^:dynamic findPivot_minVal nil)

(def ^:dynamic findPivot_ratio nil)

(def ^:dynamic findPivot_rhs nil)

(def ^:dynamic findPivot_row nil)

(def ^:dynamic findPivot_v nil)

(def ^:dynamic first_v nil)

(def ^:dynamic interpret_lastCol nil)

(def ^:dynamic interpret_nzCount nil)

(def ^:dynamic interpret_nzRow nil)

(def ^:dynamic interpret_p nil)

(def ^:dynamic interpret_result nil)

(def ^:dynamic interpret_val nil)

(def ^:dynamic pivot_factor nil)

(def ^:dynamic pivot_newRow nil)

(def ^:dynamic pivot_pivotRow nil)

(def ^:dynamic pivot_pivotVal nil)

(def ^:dynamic pivot_t nil)

(def ^:dynamic pivot_value nil)

(def ^:dynamic simplex_col nil)

(def ^:dynamic simplex_p nil)

(def ^:dynamic simplex_row nil)

(def ^:dynamic simplex_t nil)

(defn pivot [pivot_t_p pivot_row pivot_col]
  (binding [pivot_factor nil pivot_newRow nil pivot_pivotRow nil pivot_pivotVal nil pivot_t nil pivot_value nil] (try (do (set! pivot_t pivot_t_p) (set! pivot_pivotRow []) (set! pivot_pivotVal (nth (nth pivot_t pivot_row) pivot_col)) (dotimes [j (count (nth pivot_t pivot_row))] (set! pivot_pivotRow (conj pivot_pivotRow (quot (nth (nth pivot_t pivot_row) j) pivot_pivotVal)))) (set! pivot_t (assoc pivot_t pivot_row pivot_pivotRow)) (dotimes [i (count pivot_t)] (when (not= i pivot_row) (do (set! pivot_factor (nth (nth pivot_t i) pivot_col)) (set! pivot_newRow []) (dotimes [j (count (nth pivot_t i))] (do (set! pivot_value (- (nth (nth pivot_t i) j) (* pivot_factor (nth pivot_pivotRow j)))) (set! pivot_newRow (conj pivot_newRow pivot_value)))) (set! pivot_t (assoc pivot_t i pivot_newRow))))) (throw (ex-info "return" {:v pivot_t}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn findPivot [findPivot_t]
  (binding [findPivot_coeff nil findPivot_col nil findPivot_minRatio nil findPivot_minVal nil findPivot_ratio nil findPivot_rhs nil findPivot_row nil findPivot_v nil first_v nil] (try (do (set! findPivot_col 0) (set! findPivot_minVal 0.0) (dotimes [j (- (count (nth findPivot_t 0)) 1)] (do (set! findPivot_v (nth (nth findPivot_t 0) j)) (when (< findPivot_v findPivot_minVal) (do (set! findPivot_minVal findPivot_v) (set! findPivot_col j))))) (when (>= findPivot_minVal 0.0) (throw (ex-info "return" {:v [(- 1) (- 1)]}))) (set! findPivot_row (- 1)) (set! findPivot_minRatio 0.0) (set! first_v true) (doseq [i (range 1 (count findPivot_t))] (do (set! findPivot_coeff (nth (nth findPivot_t i) findPivot_col)) (when (> findPivot_coeff 0.0) (do (set! findPivot_rhs (nth (nth findPivot_t i) (- (count (nth findPivot_t i)) 1))) (set! findPivot_ratio (quot findPivot_rhs findPivot_coeff)) (when (or first_v (< findPivot_ratio findPivot_minRatio)) (do (set! findPivot_minRatio findPivot_ratio) (set! findPivot_row i) (set! first_v false))))))) (throw (ex-info "return" {:v [findPivot_row findPivot_col]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn interpret [interpret_t interpret_nVars]
  (binding [interpret_lastCol nil interpret_nzCount nil interpret_nzRow nil interpret_p nil interpret_result nil interpret_val nil] (try (do (set! interpret_lastCol (- (count (nth interpret_t 0)) 1)) (set! interpret_p (nth (nth interpret_t 0) interpret_lastCol)) (when (< interpret_p 0.0) (set! interpret_p (- interpret_p))) (set! interpret_result {}) (set! interpret_result (assoc interpret_result "P" interpret_p)) (dotimes [i interpret_nVars] (do (set! interpret_nzRow (- 1)) (set! interpret_nzCount 0) (dotimes [r (count interpret_t)] (do (set! interpret_val (nth (nth interpret_t r) i)) (when (not= interpret_val 0.0) (do (set! interpret_nzCount (+ interpret_nzCount 1)) (set! interpret_nzRow r))))) (when (and (= interpret_nzCount 1) (= (nth (nth interpret_t interpret_nzRow) i) 1.0)) (set! interpret_result (assoc interpret_result (str "x" (str (+ i 1))) (nth (nth interpret_t interpret_nzRow) interpret_lastCol)))))) (throw (ex-info "return" {:v interpret_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn simplex [simplex_tab]
  (binding [simplex_col nil simplex_p nil simplex_row nil simplex_t nil] (try (do (set! simplex_t simplex_tab) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (set! simplex_p (findPivot simplex_t)) (set! simplex_row (nth simplex_p 0)) (set! simplex_col (nth simplex_p 1)) (cond (< simplex_row 0) (recur false) :else (do (set! simplex_t (pivot simplex_t simplex_row simplex_col)) (recur while_flag_1)))))) (throw (ex-info "return" {:v simplex_t}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_tableau [[(- 1.0) (- 1.0) 0.0 0.0 0.0] [1.0 3.0 1.0 0.0 4.0] [3.0 1.0 0.0 1.0 4.0]])

(def ^:dynamic main_finalTab (simplex main_tableau))

(def ^:dynamic main_res (interpret main_finalTab 2))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "P: " (str (get main_res "P"))))
      (dotimes [i 2] (do (def ^:dynamic main_key (str "x" (str (+ i 1)))) (when (in main_key main_res) (println (str (str main_key ": ") (str (get main_res main_key)))))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
