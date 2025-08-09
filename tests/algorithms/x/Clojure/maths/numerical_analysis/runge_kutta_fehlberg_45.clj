(ns main (:refer-clojure :exclude [runge_kutta_fehlberg_45 main]))

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

(declare runge_kutta_fehlberg_45 main)

(def ^:dynamic main_y1 nil)

(def ^:dynamic main_y2 nil)

(def ^:dynamic runge_kutta_fehlberg_45_i nil)

(def ^:dynamic runge_kutta_fehlberg_45_k1 nil)

(def ^:dynamic runge_kutta_fehlberg_45_k2 nil)

(def ^:dynamic runge_kutta_fehlberg_45_k3 nil)

(def ^:dynamic runge_kutta_fehlberg_45_k4 nil)

(def ^:dynamic runge_kutta_fehlberg_45_k5 nil)

(def ^:dynamic runge_kutta_fehlberg_45_k6 nil)

(def ^:dynamic runge_kutta_fehlberg_45_n nil)

(def ^:dynamic runge_kutta_fehlberg_45_x nil)

(def ^:dynamic runge_kutta_fehlberg_45_y nil)

(def ^:dynamic runge_kutta_fehlberg_45_ys nil)

(defn runge_kutta_fehlberg_45 [runge_kutta_fehlberg_45_func runge_kutta_fehlberg_45_x_initial runge_kutta_fehlberg_45_y_initial runge_kutta_fehlberg_45_step_size runge_kutta_fehlberg_45_x_final]
  (binding [runge_kutta_fehlberg_45_i nil runge_kutta_fehlberg_45_k1 nil runge_kutta_fehlberg_45_k2 nil runge_kutta_fehlberg_45_k3 nil runge_kutta_fehlberg_45_k4 nil runge_kutta_fehlberg_45_k5 nil runge_kutta_fehlberg_45_k6 nil runge_kutta_fehlberg_45_n nil runge_kutta_fehlberg_45_x nil runge_kutta_fehlberg_45_y nil runge_kutta_fehlberg_45_ys nil] (try (do (when (>= runge_kutta_fehlberg_45_x_initial runge_kutta_fehlberg_45_x_final) (throw (Exception. "The final value of x must be greater than initial value of x."))) (when (<= runge_kutta_fehlberg_45_step_size 0.0) (throw (Exception. "Step size must be positive."))) (set! runge_kutta_fehlberg_45_n (long (quot (- runge_kutta_fehlberg_45_x_final runge_kutta_fehlberg_45_x_initial) runge_kutta_fehlberg_45_step_size))) (set! runge_kutta_fehlberg_45_ys []) (set! runge_kutta_fehlberg_45_x runge_kutta_fehlberg_45_x_initial) (set! runge_kutta_fehlberg_45_y runge_kutta_fehlberg_45_y_initial) (set! runge_kutta_fehlberg_45_ys (conj runge_kutta_fehlberg_45_ys runge_kutta_fehlberg_45_y)) (set! runge_kutta_fehlberg_45_i 0) (while (< runge_kutta_fehlberg_45_i runge_kutta_fehlberg_45_n) (do (set! runge_kutta_fehlberg_45_k1 (* runge_kutta_fehlberg_45_step_size (runge_kutta_fehlberg_45_func runge_kutta_fehlberg_45_x runge_kutta_fehlberg_45_y))) (set! runge_kutta_fehlberg_45_k2 (* runge_kutta_fehlberg_45_step_size (runge_kutta_fehlberg_45_func (+ runge_kutta_fehlberg_45_x (/ runge_kutta_fehlberg_45_step_size 4.0)) (+ runge_kutta_fehlberg_45_y (/ runge_kutta_fehlberg_45_k1 4.0))))) (set! runge_kutta_fehlberg_45_k3 (* runge_kutta_fehlberg_45_step_size (runge_kutta_fehlberg_45_func (+ runge_kutta_fehlberg_45_x (* (/ 3.0 8.0) runge_kutta_fehlberg_45_step_size)) (+ (+ runge_kutta_fehlberg_45_y (* (/ 3.0 32.0) runge_kutta_fehlberg_45_k1)) (* (/ 9.0 32.0) runge_kutta_fehlberg_45_k2))))) (set! runge_kutta_fehlberg_45_k4 (* runge_kutta_fehlberg_45_step_size (runge_kutta_fehlberg_45_func (+ runge_kutta_fehlberg_45_x (* (/ 12.0 13.0) runge_kutta_fehlberg_45_step_size)) (+ (- (+ runge_kutta_fehlberg_45_y (* (/ 1932.0 2197.0) runge_kutta_fehlberg_45_k1)) (* (/ 7200.0 2197.0) runge_kutta_fehlberg_45_k2)) (* (/ 7296.0 2197.0) runge_kutta_fehlberg_45_k3))))) (set! runge_kutta_fehlberg_45_k5 (* runge_kutta_fehlberg_45_step_size (runge_kutta_fehlberg_45_func (+ runge_kutta_fehlberg_45_x runge_kutta_fehlberg_45_step_size) (- (+ (- (+ runge_kutta_fehlberg_45_y (* (/ 439.0 216.0) runge_kutta_fehlberg_45_k1)) (* 8.0 runge_kutta_fehlberg_45_k2)) (* (/ 3680.0 513.0) runge_kutta_fehlberg_45_k3)) (* (/ 845.0 4104.0) runge_kutta_fehlberg_45_k4))))) (set! runge_kutta_fehlberg_45_k6 (* runge_kutta_fehlberg_45_step_size (runge_kutta_fehlberg_45_func (+ runge_kutta_fehlberg_45_x (/ runge_kutta_fehlberg_45_step_size 2.0)) (- (+ (- (+ (- runge_kutta_fehlberg_45_y (* (/ 8.0 27.0) runge_kutta_fehlberg_45_k1)) (* 2.0 runge_kutta_fehlberg_45_k2)) (* (/ 3544.0 2565.0) runge_kutta_fehlberg_45_k3)) (* (/ 1859.0 4104.0) runge_kutta_fehlberg_45_k4)) (* (/ 11.0 40.0) runge_kutta_fehlberg_45_k5))))) (set! runge_kutta_fehlberg_45_y (+ (- (+ (+ (+ runge_kutta_fehlberg_45_y (* (/ 16.0 135.0) runge_kutta_fehlberg_45_k1)) (* (/ 6656.0 12825.0) runge_kutta_fehlberg_45_k3)) (* (/ 28561.0 56430.0) runge_kutta_fehlberg_45_k4)) (* (/ 9.0 50.0) runge_kutta_fehlberg_45_k5)) (* (/ 2.0 55.0) runge_kutta_fehlberg_45_k6))) (set! runge_kutta_fehlberg_45_x (+ runge_kutta_fehlberg_45_x runge_kutta_fehlberg_45_step_size)) (set! runge_kutta_fehlberg_45_ys (conj runge_kutta_fehlberg_45_ys runge_kutta_fehlberg_45_y)) (set! runge_kutta_fehlberg_45_i (+ runge_kutta_fehlberg_45_i 1)))) (throw (ex-info "return" {:v runge_kutta_fehlberg_45_ys}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn f1 [f1_x f1_y]
  (try (throw (ex-info "return" {:v (+ 1.0 (* f1_y f1_y))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn f2 [f2_x f2_y]
  (try (throw (ex-info "return" {:v f2_x})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (binding [main_y1 nil main_y2 nil] (try (do (set! main_y1 (runge_kutta_fehlberg_45 f1 0.0 0.0 0.2 1.0)) (println (nth main_y1 1)) (set! main_y2 (runge_kutta_fehlberg_45 f2 (- 1.0) 0.0 0.2 0.0)) (println (nth main_y2 1))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

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
