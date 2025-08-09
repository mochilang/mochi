(ns main (:refer-clojure :exclude [abs_val validate_point manhattan_distance manhattan_distance_one_liner]))

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

(declare abs_val validate_point manhattan_distance manhattan_distance_one_liner)

(def ^:dynamic manhattan_distance_i nil)

(def ^:dynamic manhattan_distance_total nil)

(defn abs_val [abs_val_x]
  (try (if (< abs_val_x 0.0) (- abs_val_x) abs_val_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn validate_point [validate_point_p]
  (do (when (= (count validate_point_p) 0) (throw (Exception. "Missing an input"))) validate_point_p))

(defn manhattan_distance [manhattan_distance_a manhattan_distance_b]
  (binding [manhattan_distance_i nil manhattan_distance_total nil] (try (do (validate_point manhattan_distance_a) (validate_point manhattan_distance_b) (when (not= (count manhattan_distance_a) (count manhattan_distance_b)) (throw (Exception. "Both points must be in the same n-dimensional space"))) (set! manhattan_distance_total 0.0) (set! manhattan_distance_i 0) (while (< manhattan_distance_i (count manhattan_distance_a)) (do (set! manhattan_distance_total (+ manhattan_distance_total (abs_val (- (nth manhattan_distance_a manhattan_distance_i) (nth manhattan_distance_b manhattan_distance_i))))) (set! manhattan_distance_i (+ manhattan_distance_i 1)))) (throw (ex-info "return" {:v manhattan_distance_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn manhattan_distance_one_liner [manhattan_distance_one_liner_a manhattan_distance_one_liner_b]
  (try (throw (ex-info "return" {:v (manhattan_distance manhattan_distance_one_liner_a manhattan_distance_one_liner_b)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (manhattan_distance [1.0 1.0] [2.0 2.0])))
      (println (str (manhattan_distance [1.5 1.5] [2.0 2.0])))
      (println (str (manhattan_distance_one_liner [1.5 1.5] [2.5 2.0])))
      (println (str (manhattan_distance_one_liner [(- 3.0) (- 3.0) (- 3.0)] [0.0 0.0 0.0])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
