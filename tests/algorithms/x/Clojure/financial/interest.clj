(ns main (:refer-clojure :exclude [panic powf simple_interest compound_interest apr_interest main]))

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

(declare panic powf simple_interest compound_interest apr_interest main)

(def ^:dynamic powf_i nil)

(def ^:dynamic powf_result nil)

(defn panic [panic_msg]
  (println panic_msg))

(defn powf [powf_base powf_exp]
  (binding [powf_i nil powf_result nil] (try (do (set! powf_result 1.0) (set! powf_i 0) (while (< powf_i (long powf_exp)) (do (set! powf_result (* powf_result powf_base)) (set! powf_i (+ powf_i 1)))) (throw (ex-info "return" {:v powf_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn simple_interest [simple_interest_principal simple_interest_daily_rate simple_interest_days]
  (try (do (when (<= simple_interest_days 0.0) (do (panic "days_between_payments must be > 0") (throw (ex-info "return" {:v 0.0})))) (when (< simple_interest_daily_rate 0.0) (do (panic "daily_interest_rate must be >= 0") (throw (ex-info "return" {:v 0.0})))) (when (<= simple_interest_principal 0.0) (do (panic "principal must be > 0") (throw (ex-info "return" {:v 0.0})))) (throw (ex-info "return" {:v (* (* simple_interest_principal simple_interest_daily_rate) simple_interest_days)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn compound_interest [compound_interest_principal compound_interest_nominal_rate compound_interest_periods]
  (try (do (when (<= compound_interest_periods 0.0) (do (panic "number_of_compounding_periods must be > 0") (throw (ex-info "return" {:v 0.0})))) (when (< compound_interest_nominal_rate 0.0) (do (panic "nominal_annual_interest_rate_percentage must be >= 0") (throw (ex-info "return" {:v 0.0})))) (when (<= compound_interest_principal 0.0) (do (panic "principal must be > 0") (throw (ex-info "return" {:v 0.0})))) (throw (ex-info "return" {:v (* compound_interest_principal (- (powf (+ 1.0 compound_interest_nominal_rate) compound_interest_periods) 1.0))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn apr_interest [apr_interest_principal apr_interest_apr apr_interest_years]
  (try (do (when (<= apr_interest_years 0.0) (do (panic "number_of_years must be > 0") (throw (ex-info "return" {:v 0.0})))) (when (< apr_interest_apr 0.0) (do (panic "nominal_annual_percentage_rate must be >= 0") (throw (ex-info "return" {:v 0.0})))) (when (<= apr_interest_principal 0.0) (do (panic "principal must be > 0") (throw (ex-info "return" {:v 0.0})))) (throw (ex-info "return" {:v (compound_interest apr_interest_principal (/ apr_interest_apr 365.0) (* apr_interest_years 365.0))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (println (str (simple_interest 18000.0 0.06 3.0))) (println (str (simple_interest 0.5 0.06 3.0))) (println (str (simple_interest 18000.0 0.01 10.0))) (println (str (compound_interest 10000.0 0.05 3.0))) (println (str (compound_interest 10000.0 0.05 1.0))) (println (str (apr_interest 10000.0 0.05 3.0))) (println (str (apr_interest 10000.0 0.05 1.0)))))

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
