(ns main (:refer-clojure :exclude [floor pow factorial maclaurin_sin maclaurin_cos]))

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

(declare floor pow factorial maclaurin_sin maclaurin_cos)

(def ^:dynamic factorial_i nil)

(def ^:dynamic factorial_result nil)

(def ^:dynamic floor_i nil)

(def ^:dynamic maclaurin_cos_div nil)

(def ^:dynamic maclaurin_cos_power nil)

(def ^:dynamic maclaurin_cos_r nil)

(def ^:dynamic maclaurin_cos_sign nil)

(def ^:dynamic maclaurin_cos_sum nil)

(def ^:dynamic maclaurin_cos_t nil)

(def ^:dynamic maclaurin_sin_div nil)

(def ^:dynamic maclaurin_sin_power nil)

(def ^:dynamic maclaurin_sin_r nil)

(def ^:dynamic maclaurin_sin_sign nil)

(def ^:dynamic maclaurin_sin_sum nil)

(def ^:dynamic maclaurin_sin_t nil)

(def ^:dynamic pow_i nil)

(def ^:dynamic pow_result nil)

(def ^:dynamic main_PI 3.141592653589793)

(defn floor [floor_x]
  (binding [floor_i nil] (try (do (set! floor_i (long floor_x)) (when (> (double floor_i) floor_x) (set! floor_i (- floor_i 1))) (throw (ex-info "return" {:v (double floor_i)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow [pow_x pow_n]
  (binding [pow_i nil pow_result nil] (try (do (set! pow_result 1.0) (set! pow_i 0) (while (< pow_i pow_n) (do (set! pow_result (* pow_result pow_x)) (set! pow_i (+ pow_i 1)))) (throw (ex-info "return" {:v pow_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn factorial [factorial_n]
  (binding [factorial_i nil factorial_result nil] (try (do (set! factorial_result 1.0) (set! factorial_i 2) (while (<= factorial_i factorial_n) (do (set! factorial_result (* factorial_result (double factorial_i))) (set! factorial_i (+ factorial_i 1)))) (throw (ex-info "return" {:v factorial_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn maclaurin_sin [maclaurin_sin_theta maclaurin_sin_accuracy]
  (binding [maclaurin_sin_div nil maclaurin_sin_power nil maclaurin_sin_r nil maclaurin_sin_sign nil maclaurin_sin_sum nil maclaurin_sin_t nil] (try (do (set! maclaurin_sin_t maclaurin_sin_theta) (set! maclaurin_sin_div (floor (/ maclaurin_sin_t (* 2.0 main_PI)))) (set! maclaurin_sin_t (- maclaurin_sin_t (* (* 2.0 maclaurin_sin_div) main_PI))) (set! maclaurin_sin_sum 0.0) (set! maclaurin_sin_r 0) (while (< maclaurin_sin_r maclaurin_sin_accuracy) (do (set! maclaurin_sin_power (+ (* 2 maclaurin_sin_r) 1)) (set! maclaurin_sin_sign (if (= (mod maclaurin_sin_r 2) 0) 1.0 (- 1.0))) (set! maclaurin_sin_sum (+ maclaurin_sin_sum (/ (* maclaurin_sin_sign (pow maclaurin_sin_t maclaurin_sin_power)) (factorial maclaurin_sin_power)))) (set! maclaurin_sin_r (+ maclaurin_sin_r 1)))) (throw (ex-info "return" {:v maclaurin_sin_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn maclaurin_cos [maclaurin_cos_theta maclaurin_cos_accuracy]
  (binding [maclaurin_cos_div nil maclaurin_cos_power nil maclaurin_cos_r nil maclaurin_cos_sign nil maclaurin_cos_sum nil maclaurin_cos_t nil] (try (do (set! maclaurin_cos_t maclaurin_cos_theta) (set! maclaurin_cos_div (floor (/ maclaurin_cos_t (* 2.0 main_PI)))) (set! maclaurin_cos_t (- maclaurin_cos_t (* (* 2.0 maclaurin_cos_div) main_PI))) (set! maclaurin_cos_sum 0.0) (set! maclaurin_cos_r 0) (while (< maclaurin_cos_r maclaurin_cos_accuracy) (do (set! maclaurin_cos_power (* 2 maclaurin_cos_r)) (set! maclaurin_cos_sign (if (= (mod maclaurin_cos_r 2) 0) 1.0 (- 1.0))) (set! maclaurin_cos_sum (+ maclaurin_cos_sum (/ (* maclaurin_cos_sign (pow maclaurin_cos_t maclaurin_cos_power)) (factorial maclaurin_cos_power)))) (set! maclaurin_cos_r (+ maclaurin_cos_r 1)))) (throw (ex-info "return" {:v maclaurin_cos_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (maclaurin_sin 10.0 30)))
      (println (str (maclaurin_sin (- 10.0) 30)))
      (println (str (maclaurin_sin 10.0 15)))
      (println (str (maclaurin_sin (- 10.0) 15)))
      (println (str (maclaurin_cos 5.0 30)))
      (println (str (maclaurin_cos (- 5.0) 30)))
      (println (str (maclaurin_cos 10.0 15)))
      (println (str (maclaurin_cos (- 10.0) 15)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
