(ns main (:refer-clojure :exclude [floor pow10 round_to celsius_to_fahrenheit celsius_to_kelvin celsius_to_rankine fahrenheit_to_celsius fahrenheit_to_kelvin fahrenheit_to_rankine kelvin_to_celsius kelvin_to_fahrenheit kelvin_to_rankine rankine_to_celsius rankine_to_fahrenheit rankine_to_kelvin reaumur_to_kelvin reaumur_to_fahrenheit reaumur_to_celsius reaumur_to_rankine]))

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

(declare floor pow10 round_to celsius_to_fahrenheit celsius_to_kelvin celsius_to_rankine fahrenheit_to_celsius fahrenheit_to_kelvin fahrenheit_to_rankine kelvin_to_celsius kelvin_to_fahrenheit kelvin_to_rankine rankine_to_celsius rankine_to_fahrenheit rankine_to_kelvin reaumur_to_kelvin reaumur_to_fahrenheit reaumur_to_celsius reaumur_to_rankine)

(def ^:dynamic floor_i nil)

(def ^:dynamic pow10_i nil)

(def ^:dynamic pow10_p nil)

(def ^:dynamic round_to_m nil)

(defn floor [floor_x]
  (binding [floor_i nil] (try (do (set! floor_i (long floor_x)) (when (> (double floor_i) floor_x) (set! floor_i (- floor_i 1))) (throw (ex-info "return" {:v (double floor_i)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow10 [pow10_n]
  (binding [pow10_i nil pow10_p nil] (try (do (set! pow10_p 1.0) (set! pow10_i 0) (while (< pow10_i pow10_n) (do (set! pow10_p (* pow10_p 10.0)) (set! pow10_i (+ pow10_i 1)))) (throw (ex-info "return" {:v pow10_p}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn round_to [round_to_x round_to_ndigits]
  (binding [round_to_m nil] (try (do (set! round_to_m (pow10 round_to_ndigits)) (throw (ex-info "return" {:v (/ (floor (+ (* round_to_x round_to_m) 0.5)) round_to_m)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn celsius_to_fahrenheit [celsius_to_fahrenheit_c celsius_to_fahrenheit_ndigits]
  (try (throw (ex-info "return" {:v (round_to (+ (/ (* celsius_to_fahrenheit_c 9.0) 5.0) 32.0) celsius_to_fahrenheit_ndigits)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn celsius_to_kelvin [celsius_to_kelvin_c celsius_to_kelvin_ndigits]
  (try (throw (ex-info "return" {:v (round_to (+ celsius_to_kelvin_c 273.15) celsius_to_kelvin_ndigits)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn celsius_to_rankine [celsius_to_rankine_c celsius_to_rankine_ndigits]
  (try (throw (ex-info "return" {:v (round_to (+ (/ (* celsius_to_rankine_c 9.0) 5.0) 491.67) celsius_to_rankine_ndigits)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fahrenheit_to_celsius [fahrenheit_to_celsius_f fahrenheit_to_celsius_ndigits]
  (try (throw (ex-info "return" {:v (round_to (/ (* (- fahrenheit_to_celsius_f 32.0) 5.0) 9.0) fahrenheit_to_celsius_ndigits)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fahrenheit_to_kelvin [fahrenheit_to_kelvin_f fahrenheit_to_kelvin_ndigits]
  (try (throw (ex-info "return" {:v (round_to (+ (/ (* (- fahrenheit_to_kelvin_f 32.0) 5.0) 9.0) 273.15) fahrenheit_to_kelvin_ndigits)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fahrenheit_to_rankine [fahrenheit_to_rankine_f fahrenheit_to_rankine_ndigits]
  (try (throw (ex-info "return" {:v (round_to (+ fahrenheit_to_rankine_f 459.67) fahrenheit_to_rankine_ndigits)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn kelvin_to_celsius [kelvin_to_celsius_k kelvin_to_celsius_ndigits]
  (try (throw (ex-info "return" {:v (round_to (- kelvin_to_celsius_k 273.15) kelvin_to_celsius_ndigits)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn kelvin_to_fahrenheit [kelvin_to_fahrenheit_k kelvin_to_fahrenheit_ndigits]
  (try (throw (ex-info "return" {:v (round_to (+ (/ (* (- kelvin_to_fahrenheit_k 273.15) 9.0) 5.0) 32.0) kelvin_to_fahrenheit_ndigits)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn kelvin_to_rankine [kelvin_to_rankine_k kelvin_to_rankine_ndigits]
  (try (throw (ex-info "return" {:v (round_to (/ (* kelvin_to_rankine_k 9.0) 5.0) kelvin_to_rankine_ndigits)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn rankine_to_celsius [rankine_to_celsius_r rankine_to_celsius_ndigits]
  (try (throw (ex-info "return" {:v (round_to (/ (* (- rankine_to_celsius_r 491.67) 5.0) 9.0) rankine_to_celsius_ndigits)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn rankine_to_fahrenheit [rankine_to_fahrenheit_r rankine_to_fahrenheit_ndigits]
  (try (throw (ex-info "return" {:v (round_to (- rankine_to_fahrenheit_r 459.67) rankine_to_fahrenheit_ndigits)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn rankine_to_kelvin [rankine_to_kelvin_r rankine_to_kelvin_ndigits]
  (try (throw (ex-info "return" {:v (round_to (/ (* rankine_to_kelvin_r 5.0) 9.0) rankine_to_kelvin_ndigits)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn reaumur_to_kelvin [reaumur_to_kelvin_r reaumur_to_kelvin_ndigits]
  (try (throw (ex-info "return" {:v (round_to (+ (* reaumur_to_kelvin_r 1.25) 273.15) reaumur_to_kelvin_ndigits)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn reaumur_to_fahrenheit [reaumur_to_fahrenheit_r reaumur_to_fahrenheit_ndigits]
  (try (throw (ex-info "return" {:v (round_to (+ (* reaumur_to_fahrenheit_r 2.25) 32.0) reaumur_to_fahrenheit_ndigits)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn reaumur_to_celsius [reaumur_to_celsius_r reaumur_to_celsius_ndigits]
  (try (throw (ex-info "return" {:v (round_to (* reaumur_to_celsius_r 1.25) reaumur_to_celsius_ndigits)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn reaumur_to_rankine [reaumur_to_rankine_r reaumur_to_rankine_ndigits]
  (try (throw (ex-info "return" {:v (round_to (+ (+ (* reaumur_to_rankine_r 2.25) 32.0) 459.67) reaumur_to_rankine_ndigits)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (celsius_to_fahrenheit 0.0 2))
      (println (celsius_to_kelvin 0.0 2))
      (println (celsius_to_rankine 0.0 2))
      (println (fahrenheit_to_celsius 32.0 2))
      (println (fahrenheit_to_kelvin 32.0 2))
      (println (fahrenheit_to_rankine 32.0 2))
      (println (kelvin_to_celsius 273.15 2))
      (println (kelvin_to_fahrenheit 273.15 2))
      (println (kelvin_to_rankine 273.15 2))
      (println (rankine_to_celsius 491.67 2))
      (println (rankine_to_fahrenheit 491.67 2))
      (println (rankine_to_kelvin 491.67 2))
      (println (reaumur_to_kelvin 80.0 2))
      (println (reaumur_to_fahrenheit 80.0 2))
      (println (reaumur_to_celsius 80.0 2))
      (println (reaumur_to_rankine 80.0 2))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
