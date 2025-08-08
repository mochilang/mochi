(ns main (:refer-clojure :exclude [pow_float equated_monthly_installments]))

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

(declare pow_float equated_monthly_installments)

(def ^:dynamic equated_monthly_installments_factor nil)

(def ^:dynamic equated_monthly_installments_number_of_payments nil)

(def ^:dynamic equated_monthly_installments_rate_per_month nil)

(def ^:dynamic pow_float_i nil)

(def ^:dynamic pow_float_result nil)

(defn pow_float [pow_float_base pow_float_exp]
  (binding [pow_float_i nil pow_float_result nil] (try (do (set! pow_float_result 1.0) (set! pow_float_i 0) (while (< pow_float_i pow_float_exp) (do (set! pow_float_result (* pow_float_result pow_float_base)) (set! pow_float_i (+ pow_float_i 1)))) (throw (ex-info "return" {:v pow_float_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn equated_monthly_installments [equated_monthly_installments_principal equated_monthly_installments_rate_per_annum equated_monthly_installments_years_to_repay]
  (binding [equated_monthly_installments_factor nil equated_monthly_installments_number_of_payments nil equated_monthly_installments_rate_per_month nil] (try (do (when (<= equated_monthly_installments_principal 0.0) (throw (Exception. "Principal borrowed must be > 0"))) (when (< equated_monthly_installments_rate_per_annum 0.0) (throw (Exception. "Rate of interest must be >= 0"))) (when (<= equated_monthly_installments_years_to_repay 0) (throw (Exception. "Years to repay must be an integer > 0"))) (set! equated_monthly_installments_rate_per_month (/ equated_monthly_installments_rate_per_annum 12.0)) (set! equated_monthly_installments_number_of_payments (* equated_monthly_installments_years_to_repay 12)) (set! equated_monthly_installments_factor (pow_float (+ 1.0 equated_monthly_installments_rate_per_month) equated_monthly_installments_number_of_payments)) (throw (ex-info "return" {:v (quot (* (* equated_monthly_installments_principal equated_monthly_installments_rate_per_month) equated_monthly_installments_factor) (- equated_monthly_installments_factor 1.0))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (equated_monthly_installments 25000.0 0.12 3)))
      (println (str (equated_monthly_installments 25000.0 0.12 10)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
