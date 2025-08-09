(ns main (:refer-clojure :exclude [harmonic_series]))

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

(declare harmonic_series)

(def ^:dynamic harmonic_series_i nil)

(def ^:dynamic harmonic_series_limit nil)

(def ^:dynamic harmonic_series_series nil)

(defn harmonic_series [harmonic_series_n_term]
  (binding [harmonic_series_i nil harmonic_series_limit nil harmonic_series_series nil] (try (do (when (<= harmonic_series_n_term 0.0) (throw (ex-info "return" {:v []}))) (set! harmonic_series_limit (int harmonic_series_n_term)) (set! harmonic_series_series []) (set! harmonic_series_i 0) (while (< harmonic_series_i harmonic_series_limit) (do (if (= harmonic_series_i 0) (set! harmonic_series_series (conj harmonic_series_series "1")) (set! harmonic_series_series (conj harmonic_series_series (str "1/" (str (+ harmonic_series_i 1)))))) (set! harmonic_series_i (+ harmonic_series_i 1)))) (throw (ex-info "return" {:v harmonic_series_series}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (harmonic_series 5.0)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
