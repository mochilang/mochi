(ns main (:refer-clojure :exclude [absf sqrt_approx distance point_to_string test_distance main]))

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

(declare absf sqrt_approx distance point_to_string test_distance main)

(def ^:dynamic distance_dx nil)

(def ^:dynamic distance_dy nil)

(def ^:dynamic distance_dz nil)

(def ^:dynamic sqrt_approx_guess nil)

(def ^:dynamic sqrt_approx_i nil)

(def ^:dynamic test_distance_d nil)

(def ^:dynamic test_distance_p1 nil)

(def ^:dynamic test_distance_p2 nil)

(defn absf [absf_x]
  (try (if (< absf_x 0.0) (- absf_x) absf_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sqrt_approx [sqrt_approx_x]
  (binding [sqrt_approx_guess nil sqrt_approx_i nil] (try (do (when (<= sqrt_approx_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrt_approx_guess (/ sqrt_approx_x 2.0)) (set! sqrt_approx_i 0) (while (< sqrt_approx_i 20) (do (set! sqrt_approx_guess (/ (+ sqrt_approx_guess (quot sqrt_approx_x sqrt_approx_guess)) 2.0)) (set! sqrt_approx_i (+ sqrt_approx_i 1)))) (throw (ex-info "return" {:v sqrt_approx_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn distance [distance_a distance_b]
  (binding [distance_dx nil distance_dy nil distance_dz nil] (try (do (set! distance_dx (- (:x distance_b) (:x distance_a))) (set! distance_dy (- (:y distance_b) (:y distance_a))) (set! distance_dz (- (:z distance_b) (:z distance_a))) (throw (ex-info "return" {:v (sqrt_approx (absf (+ (+ (* distance_dx distance_dx) (* distance_dy distance_dy)) (* distance_dz distance_dz))))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn point_to_string [point_to_string_p]
  (try (throw (ex-info "return" {:v (str (str (str (str (str (str "Point(" (str (:x point_to_string_p))) ", ") (str (:y point_to_string_p))) ", ") (str (:z point_to_string_p))) ")")})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn test_distance []
  (binding [test_distance_d nil test_distance_p1 nil test_distance_p2 nil] (do (set! test_distance_p1 {:x 2.0 :y (- 1.0) :z 7.0}) (set! test_distance_p2 {:x 1.0 :y (- 3.0) :z 5.0}) (set! test_distance_d (distance test_distance_p1 test_distance_p2)) (when (> (absf (- test_distance_d 3.0)) 0.0001) (throw (Exception. "distance test failed"))) (println (str (str (str (str (str "Distance from " (point_to_string test_distance_p1)) " to ") (point_to_string test_distance_p2)) " is ") (str test_distance_d))))))

(defn main []
  (test_distance))

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
