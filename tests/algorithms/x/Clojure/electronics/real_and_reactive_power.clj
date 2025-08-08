(ns main (:refer-clojure :exclude [sqrt real_power reactive_power]))

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

(declare sqrt real_power reactive_power)

(def ^:dynamic sqrt_guess nil)

(def ^:dynamic sqrt_i nil)

(defn sqrt [sqrt_x]
  (binding [sqrt_guess nil sqrt_i nil] (try (do (when (<= sqrt_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrt_guess sqrt_x) (set! sqrt_i 0) (while (< sqrt_i 10) (do (set! sqrt_guess (/ (+ sqrt_guess (quot sqrt_x sqrt_guess)) 2.0)) (set! sqrt_i (+ sqrt_i 1)))) (throw (ex-info "return" {:v sqrt_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn real_power [real_power_apparent_power real_power_power_factor]
  (try (do (when (or (< real_power_power_factor (- 0.0 1.0)) (> real_power_power_factor 1.0)) (throw (Exception. "power_factor must be a valid float value between -1 and 1."))) (throw (ex-info "return" {:v (* real_power_apparent_power real_power_power_factor)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn reactive_power [reactive_power_apparent_power reactive_power_power_factor]
  (try (do (when (or (< reactive_power_power_factor (- 0.0 1.0)) (> reactive_power_power_factor 1.0)) (throw (Exception. "power_factor must be a valid float value between -1 and 1."))) (throw (ex-info "return" {:v (* reactive_power_apparent_power (sqrt (- 1.0 (* reactive_power_power_factor reactive_power_power_factor))))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (real_power 100.0 0.9)))
      (println (str (real_power 0.0 0.8)))
      (println (str (real_power 100.0 (- 0.9))))
      (println (str (reactive_power 100.0 0.9)))
      (println (str (reactive_power 0.0 0.8)))
      (println (str (reactive_power 100.0 (- 0.9))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
