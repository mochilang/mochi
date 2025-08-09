(ns main (:refer-clojure :exclude [calculate_pi test_pi_generator main]))

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

(declare calculate_pi test_pi_generator main)

(def ^:dynamic main_PI_DIGITS "1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679")

(defn calculate_pi [calculate_pi_limit]
  (try (do (when (or (< calculate_pi_limit 0) (> calculate_pi_limit (count main_PI_DIGITS))) (throw (Exception. "limit out of range"))) (throw (ex-info "return" {:v (str "3." (subs main_PI_DIGITS 0 (min calculate_pi_limit (count main_PI_DIGITS))))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn test_pi_generator []
  (when (not= (calculate_pi 15) "3.141592653589793") (throw (Exception. "calculate_pi 15 failed"))))

(defn main []
  (do (test_pi_generator) (println (calculate_pi 50))))

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
