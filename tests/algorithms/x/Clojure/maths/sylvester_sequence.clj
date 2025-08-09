(ns main (:refer-clojure :exclude [sylvester]))

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

(declare sylvester)

(def ^:dynamic sylvester_lower nil)

(def ^:dynamic sylvester_prev nil)

(def ^:dynamic sylvester_upper nil)

(defn sylvester [sylvester_n]
  (binding [sylvester_lower nil sylvester_prev nil sylvester_upper nil] (try (do (when (< sylvester_n 1) (throw (Exception. "The input value of n has to be > 0"))) (when (= sylvester_n 1) (throw (ex-info "return" {:v 2}))) (set! sylvester_prev (sylvester (- sylvester_n 1))) (set! sylvester_lower (- sylvester_prev 1)) (set! sylvester_upper sylvester_prev) (throw (ex-info "return" {:v (+ (* sylvester_lower sylvester_upper) 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (sylvester 8)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
