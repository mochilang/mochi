(ns main (:refer-clojure :exclude [pow convert_si_prefix convert_binary_prefix]))

(require 'clojure.set)

(defrecord BINARYUNITS [yotta zetta exa peta tera giga mega kilo])

(defrecord SIUNITS [yotta zetta exa peta tera giga mega kilo hecto deca deci centi milli micro nano pico femto atto zepto yocto])

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def ^:dynamic convert_binary_prefix_diff nil)

(def ^:dynamic convert_binary_prefix_kp nil)

(def ^:dynamic convert_binary_prefix_up nil)

(def ^:dynamic convert_si_prefix_diff nil)

(def ^:dynamic convert_si_prefix_kp nil)

(def ^:dynamic convert_si_prefix_up nil)

(def ^:dynamic pow_e nil)

(def ^:dynamic pow_i nil)

(def ^:dynamic pow_result nil)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare pow convert_si_prefix convert_binary_prefix)

(def ^:dynamic main_SI_UNITS {"yotta" 24 "zetta" 21 "exa" 18 "peta" 15 "tera" 12 "giga" 9 "mega" 6 "kilo" 3 "hecto" 2 "deca" 1 "deci" (- 1) "centi" (- 2) "milli" (- 3) "micro" (- 6) "nano" (- 9) "pico" (- 12) "femto" (- 15) "atto" (- 18) "zepto" (- 21) "yocto" (- 24)})

(def ^:dynamic main_BINARY_UNITS {"yotta" 8 "zetta" 7 "exa" 6 "peta" 5 "tera" 4 "giga" 3 "mega" 2 "kilo" 1})

(defn pow [pow_base pow_exp]
  (binding [pow_e nil pow_i nil pow_result nil] (try (do (when (= pow_exp 0) (throw (ex-info "return" {:v 1.0}))) (set! pow_e pow_exp) (when (< pow_e 0) (set! pow_e (- pow_e))) (set! pow_result 1.0) (set! pow_i 0) (while (< pow_i pow_e) (do (set! pow_result (* pow_result pow_base)) (set! pow_i (+ pow_i 1)))) (if (< pow_exp 0) (/ 1.0 pow_result) pow_result)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn convert_si_prefix [convert_si_prefix_known_amount convert_si_prefix_known_prefix convert_si_prefix_unknown_prefix]
  (binding [convert_si_prefix_diff nil convert_si_prefix_kp nil convert_si_prefix_up nil] (try (do (set! convert_si_prefix_kp (clojure.string/lower-case convert_si_prefix_known_prefix)) (set! convert_si_prefix_up (clojure.string/lower-case convert_si_prefix_unknown_prefix)) (when (not (in convert_si_prefix_kp main_SI_UNITS)) (throw (Exception. (str "unknown prefix: " convert_si_prefix_known_prefix)))) (when (not (in convert_si_prefix_up main_SI_UNITS)) (throw (Exception. (str "unknown prefix: " convert_si_prefix_unknown_prefix)))) (set! convert_si_prefix_diff (- (nth main_SI_UNITS convert_si_prefix_kp) (nth main_SI_UNITS convert_si_prefix_up))) (throw (ex-info "return" {:v (* convert_si_prefix_known_amount (pow 10.0 convert_si_prefix_diff))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn convert_binary_prefix [convert_binary_prefix_known_amount convert_binary_prefix_known_prefix convert_binary_prefix_unknown_prefix]
  (binding [convert_binary_prefix_diff nil convert_binary_prefix_kp nil convert_binary_prefix_up nil] (try (do (set! convert_binary_prefix_kp (clojure.string/lower-case convert_binary_prefix_known_prefix)) (set! convert_binary_prefix_up (clojure.string/lower-case convert_binary_prefix_unknown_prefix)) (when (not (in convert_binary_prefix_kp main_BINARY_UNITS)) (throw (Exception. (str "unknown prefix: " convert_binary_prefix_known_prefix)))) (when (not (in convert_binary_prefix_up main_BINARY_UNITS)) (throw (Exception. (str "unknown prefix: " convert_binary_prefix_unknown_prefix)))) (set! convert_binary_prefix_diff (* (- (nth main_BINARY_UNITS convert_binary_prefix_kp) (nth main_BINARY_UNITS convert_binary_prefix_up)) 10)) (throw (ex-info "return" {:v (* convert_binary_prefix_known_amount (pow 2.0 convert_binary_prefix_diff))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (convert_si_prefix 1.0 "giga" "mega")))
      (println (str (convert_si_prefix 1.0 "mega" "giga")))
      (println (str (convert_si_prefix 1.0 "kilo" "kilo")))
      (println (str (convert_binary_prefix 1.0 "giga" "mega")))
      (println (str (convert_binary_prefix 1.0 "mega" "giga")))
      (println (str (convert_binary_prefix 1.0 "kilo" "kilo")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
