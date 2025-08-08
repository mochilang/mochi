(ns main (:refer-clojure :exclude [ohms_law]))

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

(declare ohms_law)

(def ^:dynamic ohms_law_zeros nil)

(defn ohms_law [ohms_law_voltage ohms_law_current ohms_law_resistance]
  (binding [ohms_law_zeros nil] (try (do (set! ohms_law_zeros 0) (when (= ohms_law_voltage 0.0) (set! ohms_law_zeros (+ ohms_law_zeros 1))) (when (= ohms_law_current 0.0) (set! ohms_law_zeros (+ ohms_law_zeros 1))) (when (= ohms_law_resistance 0.0) (set! ohms_law_zeros (+ ohms_law_zeros 1))) (when (not= ohms_law_zeros 1) (do (println "One and only one argument must be 0") (throw (ex-info "return" {:v {}})))) (when (< ohms_law_resistance 0.0) (do (println "Resistance cannot be negative") (throw (ex-info "return" {:v {}})))) (when (= ohms_law_voltage 0.0) (throw (ex-info "return" {:v {"voltage" (* ohms_law_current ohms_law_resistance)}}))) (if (= ohms_law_current 0.0) {"current" (quot ohms_law_voltage ohms_law_resistance)} {"resistance" (quot ohms_law_voltage ohms_law_current)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println ((fn json_str [x] (cond (map? x) (str "{" (clojure.string/join "," (map (fn [[k v]] (str "\"" (name k) "\":" (json_str v))) x)) "}") (sequential? x) (str "[" (clojure.string/join "," (map json_str x)) "]") (string? x) (pr-str x) :else (str x))) (ohms_law 10.0 0.0 5.0)))
      (println ((fn json_str [x] (cond (map? x) (str "{" (clojure.string/join "," (map (fn [[k v]] (str "\"" (name k) "\":" (json_str v))) x)) "}") (sequential? x) (str "[" (clojure.string/join "," (map json_str x)) "]") (string? x) (pr-str x) :else (str x))) (ohms_law (- 10.0) 1.0 0.0)))
      (println ((fn json_str [x] (cond (map? x) (str "{" (clojure.string/join "," (map (fn [[k v]] (str "\"" (name k) "\":" (json_str v))) x)) "}") (sequential? x) (str "[" (clojure.string/join "," (map json_str x)) "]") (string? x) (pr-str x) :else (str x))) (ohms_law 0.0 (- 1.5) 2.0)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
