(ns main (:refer-clojure :exclude [geometric_series]))

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

(declare geometric_series)

(def ^:dynamic geometric_series_current nil)

(def ^:dynamic geometric_series_i nil)

(def ^:dynamic geometric_series_n nil)

(def ^:dynamic geometric_series_series nil)

(defn geometric_series [geometric_series_nth_term geometric_series_start_term_a geometric_series_common_ratio_r]
  (binding [geometric_series_current nil geometric_series_i nil geometric_series_n nil geometric_series_series nil] (try (do (set! geometric_series_n (long geometric_series_nth_term)) (when (or (or (<= geometric_series_n 0) (= geometric_series_start_term_a 0.0)) (= geometric_series_common_ratio_r 0.0)) (throw (ex-info "return" {:v []}))) (set! geometric_series_series []) (set! geometric_series_current geometric_series_start_term_a) (set! geometric_series_i 0) (while (< geometric_series_i geometric_series_n) (do (set! geometric_series_series (conj geometric_series_series geometric_series_current)) (set! geometric_series_current (* geometric_series_current geometric_series_common_ratio_r)) (set! geometric_series_i (+ geometric_series_i 1)))) (throw (ex-info "return" {:v geometric_series_series}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (geometric_series 4.0 2.0 2.0))
      (println (geometric_series 4.0 2.0 (- 2.0)))
      (println (geometric_series 4.0 (- 2.0) 2.0))
      (println (geometric_series (- 4.0) 2.0 2.0))
      (println (geometric_series 0.0 100.0 500.0))
      (println (geometric_series 1.0 1.0 1.0))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
