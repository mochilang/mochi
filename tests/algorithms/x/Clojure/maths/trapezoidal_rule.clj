(ns main (:refer-clojure :exclude [f make_points trapezoidal_rule]))

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

(declare f make_points trapezoidal_rule)

(def ^:dynamic make_points_x nil)

(def ^:dynamic make_points_xs nil)

(def ^:dynamic trapezoidal_rule_a nil)

(def ^:dynamic trapezoidal_rule_b nil)

(def ^:dynamic trapezoidal_rule_h nil)

(def ^:dynamic trapezoidal_rule_i nil)

(def ^:dynamic trapezoidal_rule_xs nil)

(def ^:dynamic trapezoidal_rule_y nil)

(defn f [f_x]
  (try (throw (ex-info "return" {:v (* f_x f_x)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn make_points [make_points_a make_points_b make_points_h]
  (binding [make_points_x nil make_points_xs nil] (try (do (set! make_points_xs []) (set! make_points_x (+ make_points_a make_points_h)) (while (<= make_points_x (- make_points_b make_points_h)) (do (set! make_points_xs (conj make_points_xs make_points_x)) (set! make_points_x (+ make_points_x make_points_h)))) (throw (ex-info "return" {:v make_points_xs}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn trapezoidal_rule [trapezoidal_rule_boundary trapezoidal_rule_steps]
  (binding [trapezoidal_rule_a nil trapezoidal_rule_b nil trapezoidal_rule_h nil trapezoidal_rule_i nil trapezoidal_rule_xs nil trapezoidal_rule_y nil] (try (do (set! trapezoidal_rule_h (/ (- (nth trapezoidal_rule_boundary 1) (nth trapezoidal_rule_boundary 0)) trapezoidal_rule_steps)) (set! trapezoidal_rule_a (nth trapezoidal_rule_boundary 0)) (set! trapezoidal_rule_b (nth trapezoidal_rule_boundary 1)) (set! trapezoidal_rule_xs (make_points trapezoidal_rule_a trapezoidal_rule_b trapezoidal_rule_h)) (set! trapezoidal_rule_y (* (/ trapezoidal_rule_h 2.0) (f trapezoidal_rule_a))) (set! trapezoidal_rule_i 0) (while (< trapezoidal_rule_i (count trapezoidal_rule_xs)) (do (set! trapezoidal_rule_y (+ trapezoidal_rule_y (* trapezoidal_rule_h (f (nth trapezoidal_rule_xs trapezoidal_rule_i))))) (set! trapezoidal_rule_i (+ trapezoidal_rule_i 1)))) (set! trapezoidal_rule_y (+ trapezoidal_rule_y (* (/ trapezoidal_rule_h 2.0) (f trapezoidal_rule_b)))) (throw (ex-info "return" {:v trapezoidal_rule_y}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_a 0.0)

(def ^:dynamic main_b 1.0)

(def ^:dynamic main_steps 10.0)

(def ^:dynamic main_boundary [main_a main_b])

(def ^:dynamic main_y (trapezoidal_rule main_boundary main_steps))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "y = " (str main_y)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
