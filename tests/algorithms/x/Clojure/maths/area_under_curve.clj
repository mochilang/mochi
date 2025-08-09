(ns main (:refer-clojure :exclude [abs_float trapezoidal_area f]))

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

(declare abs_float trapezoidal_area f)

(def ^:dynamic main_i nil)

(def ^:dynamic trapezoidal_area_area nil)

(def ^:dynamic trapezoidal_area_fx1 nil)

(def ^:dynamic trapezoidal_area_fx2 nil)

(def ^:dynamic trapezoidal_area_i nil)

(def ^:dynamic trapezoidal_area_step nil)

(def ^:dynamic trapezoidal_area_x1 nil)

(def ^:dynamic trapezoidal_area_x2 nil)

(defn abs_float [abs_float_x]
  (try (if (< abs_float_x 0.0) (throw (ex-info "return" {:v (- abs_float_x)})) (throw (ex-info "return" {:v abs_float_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn trapezoidal_area [f_v trapezoidal_area_x_start trapezoidal_area_x_end trapezoidal_area_steps]
  (binding [trapezoidal_area_area nil trapezoidal_area_fx1 nil trapezoidal_area_fx2 nil trapezoidal_area_i nil trapezoidal_area_step nil trapezoidal_area_x1 nil trapezoidal_area_x2 nil] (try (do (set! trapezoidal_area_step (quot (- trapezoidal_area_x_end trapezoidal_area_x_start) (double trapezoidal_area_steps))) (set! trapezoidal_area_x1 trapezoidal_area_x_start) (set! trapezoidal_area_fx1 (f_v trapezoidal_area_x_start)) (set! trapezoidal_area_area 0.0) (set! trapezoidal_area_i 0) (while (< trapezoidal_area_i trapezoidal_area_steps) (do (set! trapezoidal_area_x2 (+ trapezoidal_area_x1 trapezoidal_area_step)) (set! trapezoidal_area_fx2 (f_v trapezoidal_area_x2)) (set! trapezoidal_area_area (+ trapezoidal_area_area (/ (* (abs_float (+ trapezoidal_area_fx2 trapezoidal_area_fx1)) trapezoidal_area_step) 2.0))) (set! trapezoidal_area_x1 trapezoidal_area_x2) (set! trapezoidal_area_fx1 trapezoidal_area_fx2) (set! trapezoidal_area_i (+ trapezoidal_area_i 1)))) (throw (ex-info "return" {:v trapezoidal_area_area}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn f [f_x]
  (try (throw (ex-info "return" {:v (+ (* (* f_x f_x) f_x) (* f_x f_x))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_i 10)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println "f(x) = x^3 + x^2")
      (println "The area between the curve, x = -5, x = 5 and the x axis is:")
      (while (<= main_i 100000) (do (def ^:dynamic main_result (trapezoidal_area f (- 5.0) 5.0 main_i)) (println (str (str (str "with " (str main_i)) " steps: ") (str main_result))) (def main_i (* main_i 10))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
