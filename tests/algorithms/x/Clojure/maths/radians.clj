(ns main (:refer-clojure :exclude [radians abs_float almost_equal test_radians main]))

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

(declare radians abs_float almost_equal test_radians main)

(def ^:dynamic main_PI 3.141592653589793)

(defn radians [radians_degree]
  (try (throw (ex-info "return" {:v (/ radians_degree (/ 180.0 main_PI))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn abs_float [abs_float_x]
  (try (if (< abs_float_x 0.0) (- abs_float_x) abs_float_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn almost_equal [almost_equal_a almost_equal_b]
  (try (throw (ex-info "return" {:v (<= (abs_float (- almost_equal_a almost_equal_b)) 0.00000001)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn test_radians []
  (do (when (not (almost_equal (radians 180.0) main_PI)) (throw (Exception. "radians 180 failed"))) (when (not (almost_equal (radians 92.0) 1.6057029118347832)) (throw (Exception. "radians 92 failed"))) (when (not (almost_equal (radians 274.0) 4.782202150464463)) (throw (Exception. "radians 274 failed"))) (when (not (almost_equal (radians 109.82) 1.9167205845401725)) (throw (Exception. "radians 109.82 failed")))))

(defn main []
  (do (test_radians) (println (str (radians 180.0))) (println (str (radians 92.0))) (println (str (radians 274.0))) (println (str (radians 109.82)))))

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
