(ns main (:refer-clojure :exclude [to_float ln exp pow_float get_altitude_at_pressure]))

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

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare to_float ln exp pow_float get_altitude_at_pressure)

(def ^:dynamic exp_n nil)

(def ^:dynamic exp_sum nil)

(def ^:dynamic exp_term nil)

(def ^:dynamic get_altitude_at_pressure_ratio nil)

(def ^:dynamic ln_denom nil)

(def ^:dynamic ln_k nil)

(def ^:dynamic ln_sum nil)

(def ^:dynamic ln_term nil)

(def ^:dynamic ln_y nil)

(def ^:dynamic ln_y2 nil)

(defn to_float [to_float_x]
  (try (throw (ex-info "return" {:v (* to_float_x 1.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn ln [ln_x]
  (binding [ln_denom nil ln_k nil ln_sum nil ln_term nil ln_y nil ln_y2 nil] (try (do (when (<= ln_x 0.0) (throw (Exception. "ln domain error"))) (set! ln_y (/ (- ln_x 1.0) (+ ln_x 1.0))) (set! ln_y2 (* ln_y ln_y)) (set! ln_term ln_y) (set! ln_sum 0.0) (set! ln_k 0) (while (< ln_k 10) (do (set! ln_denom (to_float (+ (* 2 ln_k) 1))) (set! ln_sum (+ ln_sum (quot ln_term ln_denom))) (set! ln_term (* ln_term ln_y2)) (set! ln_k (+ ln_k 1)))) (throw (ex-info "return" {:v (* 2.0 ln_sum)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn exp [exp_x]
  (binding [exp_n nil exp_sum nil exp_term nil] (try (do (set! exp_term 1.0) (set! exp_sum 1.0) (set! exp_n 1) (while (< exp_n 20) (do (set! exp_term (/ (* exp_term exp_x) (to_float exp_n))) (set! exp_sum (+ exp_sum exp_term)) (set! exp_n (+ exp_n 1)))) (throw (ex-info "return" {:v exp_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow_float [pow_float_base pow_float_exponent]
  (try (throw (ex-info "return" {:v (exp (* pow_float_exponent (ln pow_float_base)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn get_altitude_at_pressure [get_altitude_at_pressure_pressure]
  (binding [get_altitude_at_pressure_ratio nil] (try (do (when (> get_altitude_at_pressure_pressure 101325.0) (throw (Exception. "Value Higher than Pressure at Sea Level !"))) (when (< get_altitude_at_pressure_pressure 0.0) (throw (Exception. "Atmospheric Pressure can not be negative !"))) (set! get_altitude_at_pressure_ratio (/ get_altitude_at_pressure_pressure 101325.0)) (throw (ex-info "return" {:v (* 44330.0 (- 1.0 (pow_float get_altitude_at_pressure_ratio (/ 1.0 5.5255))))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (mochi_str (get_altitude_at_pressure 100000.0)))
      (println (mochi_str (get_altitude_at_pressure 101325.0)))
      (println (mochi_str (get_altitude_at_pressure 80000.0)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
