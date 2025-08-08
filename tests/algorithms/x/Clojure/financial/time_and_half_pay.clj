(ns main (:refer-clojure :exclude [pay main]))

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

(declare pay main)

(def ^:dynamic pay_normal_pay nil)

(def ^:dynamic pay_over_time nil)

(def ^:dynamic pay_over_time_pay nil)

(defn pay [pay_hours_worked pay_pay_rate pay_hours]
  (binding [pay_normal_pay nil pay_over_time nil pay_over_time_pay nil] (try (do (set! pay_normal_pay (* pay_hours_worked pay_pay_rate)) (set! pay_over_time (- pay_hours_worked pay_hours)) (when (< pay_over_time 0.0) (set! pay_over_time 0.0)) (set! pay_over_time_pay (/ (* pay_over_time pay_pay_rate) 2.0)) (throw (ex-info "return" {:v (+ pay_normal_pay pay_over_time_pay)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (str (pay 41.0 1.0 40.0))) (println (str (pay 65.0 19.0 40.0))) (println (str (pay 10.0 1.0 40.0)))))

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
