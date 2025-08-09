(ns main (:refer-clojure :exclude [pow_string p_series]))

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

(declare pow_string p_series)

(def ^:dynamic p_series_i nil)

(def ^:dynamic p_series_series nil)

(def ^:dynamic pow_string_b nil)

(def ^:dynamic pow_string_e nil)

(def ^:dynamic pow_string_i nil)

(def ^:dynamic pow_string_res nil)

(def ^:dynamic pow_string_value nil)

(defn pow_string [pow_string_base pow_string_exp]
  (binding [pow_string_b nil pow_string_e nil pow_string_i nil pow_string_res nil pow_string_value nil] (try (do (when (>= pow_string_exp 0) (do (set! pow_string_res 1) (set! pow_string_i 0) (while (< pow_string_i pow_string_exp) (do (set! pow_string_res (* pow_string_res pow_string_base)) (set! pow_string_i (+ pow_string_i 1)))) (throw (ex-info "return" {:v (str pow_string_res)})))) (set! pow_string_e (- pow_string_exp)) (set! pow_string_res 1.0) (set! pow_string_b (* pow_string_base 1.0)) (set! pow_string_i 0) (while (< pow_string_i pow_string_e) (do (set! pow_string_res (* pow_string_res pow_string_b)) (set! pow_string_i (+ pow_string_i 1)))) (set! pow_string_value (/ 1.0 pow_string_res)) (throw (ex-info "return" {:v (str pow_string_value)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn p_series [p_series_nth_term p_series_power]
  (binding [p_series_i nil p_series_series nil] (try (do (set! p_series_series []) (when (<= p_series_nth_term 0) (throw (ex-info "return" {:v p_series_series}))) (set! p_series_i 1) (while (<= p_series_i p_series_nth_term) (do (if (= p_series_i 1) (set! p_series_series (conj p_series_series "1")) (set! p_series_series (conj p_series_series (str "1 / " (pow_string p_series_i p_series_power))))) (set! p_series_i (+ p_series_i 1)))) (throw (ex-info "return" {:v p_series_series}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (p_series 5 2))
      (println (p_series (- 5) 2))
      (println (p_series 5 (- 2)))
      (println (p_series 0 0))
      (println (p_series 1 1))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
