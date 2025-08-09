(ns main (:refer-clojure :exclude [abs_int gcd power is_carmichael_number]))

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

(declare abs_int gcd power is_carmichael_number)

(def ^:dynamic is_carmichael_number_b nil)

(def ^:dynamic power_temp nil)

(defn abs_int [abs_int_x]
  (try (if (< abs_int_x 0) (- abs_int_x) abs_int_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn gcd [gcd_a gcd_b]
  (try (if (= gcd_a 0) (abs_int gcd_b) (gcd (mod gcd_b gcd_a) gcd_a)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn power [power_x power_y power_m]
  (binding [power_temp nil] (try (do (when (= power_y 0) (throw (ex-info "return" {:v (mod 1 power_m)}))) (set! power_temp (mod (power power_x (quot power_y 2) power_m) power_m)) (set! power_temp (mod (* power_temp power_temp) power_m)) (when (= (mod power_y 2) 1) (set! power_temp (mod (* power_temp power_x) power_m))) (throw (ex-info "return" {:v power_temp}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_carmichael_number [is_carmichael_number_n]
  (binding [is_carmichael_number_b nil] (try (do (when (<= is_carmichael_number_n 0) (throw (Exception. "Number must be positive"))) (set! is_carmichael_number_b 2) (while (< is_carmichael_number_b is_carmichael_number_n) (do (when (= (gcd is_carmichael_number_b is_carmichael_number_n) 1) (when (not= (power is_carmichael_number_b (- is_carmichael_number_n 1) is_carmichael_number_n) 1) (throw (ex-info "return" {:v false})))) (set! is_carmichael_number_b (+ is_carmichael_number_b 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (power 2 15 3)))
      (println (str (power 5 1 30)))
      (println (str (is_carmichael_number 4)))
      (println (str (is_carmichael_number 561)))
      (println (str (is_carmichael_number 562)))
      (println (str (is_carmichael_number 1105)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
