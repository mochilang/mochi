(ns main (:refer-clojure :exclude [int_sqrt is_pronic test_is_pronic main]))

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

(declare int_sqrt is_pronic test_is_pronic main)

(def ^:dynamic int_sqrt_r nil)

(def ^:dynamic is_pronic_root nil)

(defn int_sqrt [int_sqrt_n]
  (binding [int_sqrt_r nil] (try (do (set! int_sqrt_r 0) (while (<= (* (+ int_sqrt_r 1) (+ int_sqrt_r 1)) int_sqrt_n) (set! int_sqrt_r (+ int_sqrt_r 1))) (throw (ex-info "return" {:v int_sqrt_r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_pronic [is_pronic_n]
  (binding [is_pronic_root nil] (try (do (when (< is_pronic_n 0) (throw (ex-info "return" {:v false}))) (when (not= (mod is_pronic_n 2) 0) (throw (ex-info "return" {:v false}))) (set! is_pronic_root (int_sqrt is_pronic_n)) (throw (ex-info "return" {:v (= is_pronic_n (* is_pronic_root (+ is_pronic_root 1)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_is_pronic []
  (do (when (is_pronic (- 1)) (throw (Exception. "-1 should not be pronic"))) (when (not (is_pronic 0)) (throw (Exception. "0 should be pronic"))) (when (not (is_pronic 2)) (throw (Exception. "2 should be pronic"))) (when (is_pronic 5) (throw (Exception. "5 should not be pronic"))) (when (not (is_pronic 6)) (throw (Exception. "6 should be pronic"))) (when (is_pronic 8) (throw (Exception. "8 should not be pronic"))) (when (not (is_pronic 30)) (throw (Exception. "30 should be pronic"))) (when (is_pronic 32) (throw (Exception. "32 should not be pronic"))) (when (not (is_pronic 2147441940)) (throw (Exception. "2147441940 should be pronic")))))

(defn main []
  (do (test_is_pronic) (println (is_pronic 56))))

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
