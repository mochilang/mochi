(ns main (:refer-clojure :exclude [straight_line_depreciation]))

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

(declare straight_line_depreciation)

(def ^:dynamic straight_line_depreciation_accumulated nil)

(def ^:dynamic straight_line_depreciation_annual_expense nil)

(def ^:dynamic straight_line_depreciation_depreciable_cost nil)

(def ^:dynamic straight_line_depreciation_end_year_expense nil)

(def ^:dynamic straight_line_depreciation_expenses nil)

(def ^:dynamic straight_line_depreciation_period nil)

(defn straight_line_depreciation [straight_line_depreciation_useful_years straight_line_depreciation_purchase_value straight_line_depreciation_residual_value]
  (binding [straight_line_depreciation_accumulated nil straight_line_depreciation_annual_expense nil straight_line_depreciation_depreciable_cost nil straight_line_depreciation_end_year_expense nil straight_line_depreciation_expenses nil straight_line_depreciation_period nil] (try (do (when (< straight_line_depreciation_useful_years 1) (throw (Exception. "Useful years cannot be less than 1"))) (when (< straight_line_depreciation_purchase_value 0.0) (throw (Exception. "Purchase value cannot be less than zero"))) (when (< straight_line_depreciation_purchase_value straight_line_depreciation_residual_value) (throw (Exception. "Purchase value cannot be less than residual value"))) (set! straight_line_depreciation_depreciable_cost (- straight_line_depreciation_purchase_value straight_line_depreciation_residual_value)) (set! straight_line_depreciation_annual_expense (quot straight_line_depreciation_depreciable_cost (* 1.0 straight_line_depreciation_useful_years))) (set! straight_line_depreciation_expenses []) (set! straight_line_depreciation_accumulated 0.0) (set! straight_line_depreciation_period 0) (while (< straight_line_depreciation_period straight_line_depreciation_useful_years) (do (if (not= straight_line_depreciation_period (- straight_line_depreciation_useful_years 1)) (do (set! straight_line_depreciation_accumulated (+ straight_line_depreciation_accumulated straight_line_depreciation_annual_expense)) (set! straight_line_depreciation_expenses (conj straight_line_depreciation_expenses straight_line_depreciation_annual_expense))) (do (set! straight_line_depreciation_end_year_expense (- straight_line_depreciation_depreciable_cost straight_line_depreciation_accumulated)) (set! straight_line_depreciation_expenses (conj straight_line_depreciation_expenses straight_line_depreciation_end_year_expense)))) (set! straight_line_depreciation_period (+ straight_line_depreciation_period 1)))) (throw (ex-info "return" {:v straight_line_depreciation_expenses}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (straight_line_depreciation 10 1100.0 100.0)))
      (println (str (straight_line_depreciation 6 1250.0 50.0)))
      (println (str (straight_line_depreciation 4 1001.0 0.0)))
      (println (str (straight_line_depreciation 11 380.0 50.0)))
      (println (str (straight_line_depreciation 1 4985.0 100.0)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
