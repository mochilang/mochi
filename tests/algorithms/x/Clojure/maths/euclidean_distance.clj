(ns main (:refer-clojure :exclude [sqrtApprox euclidean_distance euclidean_distance_no_np main]))

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

(declare sqrtApprox euclidean_distance euclidean_distance_no_np main)

(def ^:dynamic euclidean_distance_diff nil)

(def ^:dynamic euclidean_distance_i nil)

(def ^:dynamic euclidean_distance_sum nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (when (<= sqrtApprox_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrtApprox_guess sqrtApprox_x) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (quot sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn euclidean_distance [euclidean_distance_v1 euclidean_distance_v2]
  (binding [euclidean_distance_diff nil euclidean_distance_i nil euclidean_distance_sum nil] (try (do (set! euclidean_distance_sum 0.0) (set! euclidean_distance_i 0) (while (< euclidean_distance_i (count euclidean_distance_v1)) (do (set! euclidean_distance_diff (- (nth euclidean_distance_v1 euclidean_distance_i) (nth euclidean_distance_v2 euclidean_distance_i))) (set! euclidean_distance_sum (+ euclidean_distance_sum (* euclidean_distance_diff euclidean_distance_diff))) (set! euclidean_distance_i (+ euclidean_distance_i 1)))) (throw (ex-info "return" {:v (sqrtApprox euclidean_distance_sum)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn euclidean_distance_no_np [euclidean_distance_no_np_v1 euclidean_distance_no_np_v2]
  (try (throw (ex-info "return" {:v (euclidean_distance euclidean_distance_no_np_v1 euclidean_distance_no_np_v2)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (println (str (euclidean_distance [0.0 0.0] [2.0 2.0]))) (println (str (euclidean_distance [0.0 0.0 0.0] [2.0 2.0 2.0]))) (println (str (euclidean_distance [1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0]))) (println (str (euclidean_distance_no_np [1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0]))) (println (str (euclidean_distance_no_np [0.0 0.0] [2.0 2.0])))))

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
