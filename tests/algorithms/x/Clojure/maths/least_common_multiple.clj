(ns main (:refer-clojure :exclude [gcd lcm_slow lcm_fast]))

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

(declare gcd lcm_slow lcm_fast)

(def ^:dynamic gcd_temp nil)

(def ^:dynamic gcd_x nil)

(def ^:dynamic gcd_y nil)

(def ^:dynamic lcm_slow_max nil)

(def ^:dynamic lcm_slow_multiple nil)

(defn gcd [gcd_a gcd_b]
  (binding [gcd_temp nil gcd_x nil gcd_y nil] (try (do (set! gcd_x (if (>= gcd_a 0) gcd_a (- gcd_a))) (set! gcd_y (if (>= gcd_b 0) gcd_b (- gcd_b))) (while (not= gcd_y 0) (do (set! gcd_temp (mod gcd_x gcd_y)) (set! gcd_x gcd_y) (set! gcd_y gcd_temp))) (throw (ex-info "return" {:v gcd_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn lcm_slow [lcm_slow_a lcm_slow_b]
  (binding [lcm_slow_max nil lcm_slow_multiple nil] (try (do (set! lcm_slow_max (if (>= lcm_slow_a lcm_slow_b) lcm_slow_a lcm_slow_b)) (set! lcm_slow_multiple lcm_slow_max) (while (or (not= (mod lcm_slow_multiple lcm_slow_a) 0) (not= (mod lcm_slow_multiple lcm_slow_b) 0)) (set! lcm_slow_multiple (+ lcm_slow_multiple lcm_slow_max))) (throw (ex-info "return" {:v lcm_slow_multiple}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn lcm_fast [lcm_fast_a lcm_fast_b]
  (try (throw (ex-info "return" {:v (* (quot lcm_fast_a (gcd lcm_fast_a lcm_fast_b)) lcm_fast_b)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (lcm_slow 5 2)))
      (println (str (lcm_slow 12 76)))
      (println (str (lcm_fast 5 2)))
      (println (str (lcm_fast 12 76)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
