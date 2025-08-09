(ns main (:refer-clojure :exclude [signum test_signum main]))

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

(declare signum test_signum main)

(defn signum [signum_num]
  (try (do (when (< signum_num 0.0) (throw (ex-info "return" {:v (- 1)}))) (if (> signum_num 0.0) 1 0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn test_signum []
  (do (when (not= (signum 5.0) 1) (throw (Exception. "signum(5) failed"))) (when (not= (signum (- 5.0)) (- 1)) (throw (Exception. "signum(-5) failed"))) (when (not= (signum 0.0) 0) (throw (Exception. "signum(0) failed"))) (when (not= (signum 10.5) 1) (throw (Exception. "signum(10.5) failed"))) (when (not= (signum (- 10.5)) (- 1)) (throw (Exception. "signum(-10.5) failed"))) (when (not= (signum 0.000001) 1) (throw (Exception. "signum(1e-6) failed"))) (when (not= (signum (- 0.000001)) (- 1)) (throw (Exception. "signum(-1e-6) failed"))) (when (not= (signum 123456789.0) 1) (throw (Exception. "signum(123456789) failed"))) (when (not= (signum (- 123456789.0)) (- 1)) (throw (Exception. "signum(-123456789) failed")))))

(defn main []
  (do (test_signum) (println (signum 12.0)) (println (signum (- 12.0))) (println (signum 0.0))))

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
