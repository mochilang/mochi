(ns main (:refer-clojure :exclude [abs chebyshev_distance]))

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

(declare abs chebyshev_distance)

(def ^:dynamic chebyshev_distance_diff nil)

(def ^:dynamic chebyshev_distance_i nil)

(def ^:dynamic chebyshev_distance_max_diff nil)

(defn abs [abs_x]
  (try (if (>= abs_x 0.0) (throw (ex-info "return" {:v abs_x})) (throw (ex-info "return" {:v (- abs_x)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn chebyshev_distance [chebyshev_distance_point_a chebyshev_distance_point_b]
  (binding [chebyshev_distance_diff nil chebyshev_distance_i nil chebyshev_distance_max_diff nil] (try (do (when (not= (count chebyshev_distance_point_a) (count chebyshev_distance_point_b)) (throw (Exception. "Both points must have the same dimension."))) (set! chebyshev_distance_max_diff 0.0) (set! chebyshev_distance_i 0) (while (< chebyshev_distance_i (count chebyshev_distance_point_a)) (do (set! chebyshev_distance_diff (abs (- (nth chebyshev_distance_point_a chebyshev_distance_i) (nth chebyshev_distance_point_b chebyshev_distance_i)))) (when (> chebyshev_distance_diff chebyshev_distance_max_diff) (set! chebyshev_distance_max_diff chebyshev_distance_diff)) (set! chebyshev_distance_i (+ chebyshev_distance_i 1)))) (throw (ex-info "return" {:v chebyshev_distance_max_diff}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (chebyshev_distance [1.0 1.0] [2.0 2.0]))
      (println (chebyshev_distance [1.0 1.0 9.0] [2.0 2.0 (- 5.2)]))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
