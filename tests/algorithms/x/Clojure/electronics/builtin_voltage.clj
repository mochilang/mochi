(ns main (:refer-clojure :exclude [pow10 ln_series ln builtin_voltage]))

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

(declare pow10 ln_series ln builtin_voltage)

(def ^:dynamic ln_k nil)

(def ^:dynamic ln_series_n nil)

(def ^:dynamic ln_series_sum nil)

(def ^:dynamic ln_series_t nil)

(def ^:dynamic ln_series_term nil)

(def ^:dynamic ln_y nil)

(def ^:dynamic pow10_i nil)

(def ^:dynamic pow10_result nil)

(defn pow10 [pow10_n]
  (binding [pow10_i nil pow10_result nil] (try (do (set! pow10_result 1.0) (set! pow10_i 0) (while (< pow10_i pow10_n) (do (set! pow10_result (* pow10_result 10.0)) (set! pow10_i (+ pow10_i 1)))) (throw (ex-info "return" {:v pow10_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_BOLTZMANN (/ 1.380649 (pow10 23)))

(def ^:dynamic main_ELECTRON_VOLT (/ 1.602176634 (pow10 19)))

(def ^:dynamic main_TEMPERATURE 300.0)

(defn ln_series [ln_series_x]
  (binding [ln_series_n nil ln_series_sum nil ln_series_t nil ln_series_term nil] (try (do (set! ln_series_t (quot (- ln_series_x 1.0) (+ ln_series_x 1.0))) (set! ln_series_term ln_series_t) (set! ln_series_sum 0.0) (set! ln_series_n 1) (while (<= ln_series_n 19) (do (set! ln_series_sum (+ ln_series_sum (quot ln_series_term (double ln_series_n)))) (set! ln_series_term (* (* ln_series_term ln_series_t) ln_series_t)) (set! ln_series_n (+ ln_series_n 2)))) (throw (ex-info "return" {:v (* 2.0 ln_series_sum)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn ln [ln_x]
  (binding [ln_k nil ln_y nil] (try (do (set! ln_y ln_x) (set! ln_k 0) (while (>= ln_y 10.0) (do (set! ln_y (/ ln_y 10.0)) (set! ln_k (+ ln_k 1)))) (while (< ln_y 1.0) (do (set! ln_y (* ln_y 10.0)) (set! ln_k (- ln_k 1)))) (throw (ex-info "return" {:v (+ (ln_series ln_y) (* (double ln_k) (ln_series 10.0)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn builtin_voltage [builtin_voltage_donor_conc builtin_voltage_acceptor_conc builtin_voltage_intrinsic_conc]
  (try (do (when (<= builtin_voltage_donor_conc 0.0) (throw (Exception. "Donor concentration should be positive"))) (when (<= builtin_voltage_acceptor_conc 0.0) (throw (Exception. "Acceptor concentration should be positive"))) (when (<= builtin_voltage_intrinsic_conc 0.0) (throw (Exception. "Intrinsic concentration should be positive"))) (when (<= builtin_voltage_donor_conc builtin_voltage_intrinsic_conc) (throw (Exception. "Donor concentration should be greater than intrinsic concentration"))) (when (<= builtin_voltage_acceptor_conc builtin_voltage_intrinsic_conc) (throw (Exception. "Acceptor concentration should be greater than intrinsic concentration"))) (throw (ex-info "return" {:v (/ (* (* main_BOLTZMANN main_TEMPERATURE) (ln (quot (* builtin_voltage_donor_conc builtin_voltage_acceptor_conc) (* builtin_voltage_intrinsic_conc builtin_voltage_intrinsic_conc)))) main_ELECTRON_VOLT)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (builtin_voltage (pow10 17) (pow10 17) (pow10 10))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
