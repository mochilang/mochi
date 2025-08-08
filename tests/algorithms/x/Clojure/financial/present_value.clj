(ns main (:refer-clojure :exclude [powf round2 present_value]))

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

(declare powf round2 present_value)

(def ^:dynamic powf_i nil)

(def ^:dynamic powf_result nil)

(def ^:dynamic present_value_cf nil)

(def ^:dynamic present_value_factor nil)

(def ^:dynamic present_value_i nil)

(def ^:dynamic present_value_pv nil)

(def ^:dynamic round2_scaled nil)

(defn powf [powf_base powf_exponent]
  (binding [powf_i nil powf_result nil] (try (do (set! powf_result 1.0) (set! powf_i 0) (while (< powf_i powf_exponent) (do (set! powf_result (* powf_result powf_base)) (set! powf_i (+ powf_i 1)))) (throw (ex-info "return" {:v powf_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn round2 [round2_value]
  (binding [round2_scaled nil] (try (do (when (>= round2_value 0.0) (do (set! round2_scaled (long (+ (* round2_value 100.0) 0.5))) (throw (ex-info "return" {:v (/ (double round2_scaled) 100.0)})))) (set! round2_scaled (long (- (* round2_value 100.0) 0.5))) (throw (ex-info "return" {:v (/ (double round2_scaled) 100.0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn present_value [present_value_discount_rate present_value_cash_flows]
  (binding [present_value_cf nil present_value_factor nil present_value_i nil present_value_pv nil] (try (do (when (< present_value_discount_rate 0.0) (throw (Exception. "Discount rate cannot be negative"))) (when (= (count present_value_cash_flows) 0) (throw (Exception. "Cash flows list cannot be empty"))) (set! present_value_pv 0.0) (set! present_value_i 0) (set! present_value_factor (+ 1.0 present_value_discount_rate)) (while (< present_value_i (count present_value_cash_flows)) (do (set! present_value_cf (nth present_value_cash_flows present_value_i)) (set! present_value_pv (+ present_value_pv (quot present_value_cf (powf present_value_factor present_value_i)))) (set! present_value_i (+ present_value_i 1)))) (throw (ex-info "return" {:v (round2 present_value_pv)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (present_value 0.13 [10.0 20.7 (- 293.0) 297.0])))
      (println (str (present_value 0.07 [(- 109129.39) 30923.23 15098.93 29734.0 39.0])))
      (println (str (present_value 0.07 [109129.39 30923.23 15098.93 29734.0 39.0])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
