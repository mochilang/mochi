(ns main (:refer-clojure :exclude [rand random hypercube_points]))

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

(declare rand random hypercube_points)

(def ^:dynamic hypercube_points_i nil)

(def ^:dynamic hypercube_points_j nil)

(def ^:dynamic hypercube_points_point nil)

(def ^:dynamic hypercube_points_points nil)

(def ^:dynamic hypercube_points_value nil)

(def ^:dynamic main_seed 1)

(defn rand []
  (try (do (alter-var-root (var main_seed) (fn [_] (mod (+ (* main_seed 1103515245) 12345) 2147483648))) (throw (ex-info "return" {:v main_seed}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn random []
  (try (throw (ex-info "return" {:v (/ (double (rand)) 2147483648.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn hypercube_points [hypercube_points_num_points hypercube_points_hypercube_size hypercube_points_num_dimensions]
  (binding [hypercube_points_i nil hypercube_points_j nil hypercube_points_point nil hypercube_points_points nil hypercube_points_value nil] (try (do (set! hypercube_points_points []) (set! hypercube_points_i 0) (while (< hypercube_points_i hypercube_points_num_points) (do (set! hypercube_points_point []) (set! hypercube_points_j 0) (while (< hypercube_points_j hypercube_points_num_dimensions) (do (set! hypercube_points_value (* hypercube_points_hypercube_size (random))) (set! hypercube_points_point (conj hypercube_points_point hypercube_points_value)) (set! hypercube_points_j (+ hypercube_points_j 1)))) (set! hypercube_points_points (conj hypercube_points_points hypercube_points_point)) (set! hypercube_points_i (+ hypercube_points_i 1)))) (throw (ex-info "return" {:v hypercube_points_points}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_pts (hypercube_points 3 1.0 2))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println main_pts)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
