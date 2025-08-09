(ns main (:refer-clojure :exclude [sqrt runge_kutta_gills f1 f2]))

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

(declare sqrt runge_kutta_gills f1 f2)

(def ^:dynamic runge_kutta_gills_i nil)

(def ^:dynamic runge_kutta_gills_idx nil)

(def ^:dynamic runge_kutta_gills_k1 nil)

(def ^:dynamic runge_kutta_gills_k2 nil)

(def ^:dynamic runge_kutta_gills_k3 nil)

(def ^:dynamic runge_kutta_gills_k4 nil)

(def ^:dynamic runge_kutta_gills_n nil)

(def ^:dynamic runge_kutta_gills_root2 nil)

(def ^:dynamic runge_kutta_gills_xi nil)

(def ^:dynamic runge_kutta_gills_y nil)

(def ^:dynamic sqrt_guess nil)

(def ^:dynamic sqrt_i nil)

(defn sqrt [sqrt_x]
  (binding [sqrt_guess nil sqrt_i nil] (try (do (set! sqrt_guess (if (> sqrt_x 1.0) (/ sqrt_x 2.0) 1.0)) (set! sqrt_i 0) (while (< sqrt_i 20) (do (set! sqrt_guess (* 0.5 (+ sqrt_guess (quot sqrt_x sqrt_guess)))) (set! sqrt_i (+ sqrt_i 1)))) (throw (ex-info "return" {:v sqrt_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn runge_kutta_gills [runge_kutta_gills_func runge_kutta_gills_x_initial runge_kutta_gills_y_initial runge_kutta_gills_step_size runge_kutta_gills_x_final]
  (binding [runge_kutta_gills_i nil runge_kutta_gills_idx nil runge_kutta_gills_k1 nil runge_kutta_gills_k2 nil runge_kutta_gills_k3 nil runge_kutta_gills_k4 nil runge_kutta_gills_n nil runge_kutta_gills_root2 nil runge_kutta_gills_xi nil runge_kutta_gills_y nil] (try (do (when (>= runge_kutta_gills_x_initial runge_kutta_gills_x_final) (throw (Exception. "The final value of x must be greater than initial value of x."))) (when (<= runge_kutta_gills_step_size 0.0) (throw (Exception. "Step size must be positive."))) (set! runge_kutta_gills_n (long (quot (- runge_kutta_gills_x_final runge_kutta_gills_x_initial) runge_kutta_gills_step_size))) (set! runge_kutta_gills_y []) (set! runge_kutta_gills_i 0) (while (<= runge_kutta_gills_i runge_kutta_gills_n) (do (set! runge_kutta_gills_y (conj runge_kutta_gills_y 0.0)) (set! runge_kutta_gills_i (+ runge_kutta_gills_i 1)))) (set! runge_kutta_gills_y (assoc runge_kutta_gills_y 0 runge_kutta_gills_y_initial)) (set! runge_kutta_gills_xi runge_kutta_gills_x_initial) (set! runge_kutta_gills_idx 0) (set! runge_kutta_gills_root2 (sqrt 2.0)) (while (< runge_kutta_gills_idx runge_kutta_gills_n) (do (set! runge_kutta_gills_k1 (* runge_kutta_gills_step_size (runge_kutta_gills_func runge_kutta_gills_xi (nth runge_kutta_gills_y runge_kutta_gills_idx)))) (set! runge_kutta_gills_k2 (* runge_kutta_gills_step_size (runge_kutta_gills_func (+ runge_kutta_gills_xi (/ runge_kutta_gills_step_size 2.0)) (+ (nth runge_kutta_gills_y runge_kutta_gills_idx) (/ runge_kutta_gills_k1 2.0))))) (set! runge_kutta_gills_k3 (* runge_kutta_gills_step_size (runge_kutta_gills_func (+ runge_kutta_gills_xi (/ runge_kutta_gills_step_size 2.0)) (+ (+ (nth runge_kutta_gills_y runge_kutta_gills_idx) (* (+ (- 0.5) (/ 1.0 runge_kutta_gills_root2)) runge_kutta_gills_k1)) (* (- 1.0 (/ 1.0 runge_kutta_gills_root2)) runge_kutta_gills_k2))))) (set! runge_kutta_gills_k4 (* runge_kutta_gills_step_size (runge_kutta_gills_func (+ runge_kutta_gills_xi runge_kutta_gills_step_size) (+ (- (nth runge_kutta_gills_y runge_kutta_gills_idx) (* (/ 1.0 runge_kutta_gills_root2) runge_kutta_gills_k2)) (* (+ 1.0 (/ 1.0 runge_kutta_gills_root2)) runge_kutta_gills_k3))))) (set! runge_kutta_gills_y (assoc runge_kutta_gills_y (+ runge_kutta_gills_idx 1) (+ (nth runge_kutta_gills_y runge_kutta_gills_idx) (/ (+ (+ (+ runge_kutta_gills_k1 (* (- 2.0 runge_kutta_gills_root2) runge_kutta_gills_k2)) (* (+ 2.0 runge_kutta_gills_root2) runge_kutta_gills_k3)) runge_kutta_gills_k4) 6.0)))) (set! runge_kutta_gills_xi (+ runge_kutta_gills_xi runge_kutta_gills_step_size)) (set! runge_kutta_gills_idx (+ runge_kutta_gills_idx 1)))) (throw (ex-info "return" {:v runge_kutta_gills_y}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn f1 [f1_x f1_y]
  (try (throw (ex-info "return" {:v (/ (- f1_x f1_y) 2.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_y1 (runge_kutta_gills f1 0.0 3.0 0.2 5.0))

(defn f2 [f2_x f2_y]
  (try (throw (ex-info "return" {:v f2_x})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_y2 (runge_kutta_gills f2 (- 1.0) 0.0 0.2 0.0))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (nth main_y1 (- (count main_y1) 1))))
      (println (str main_y2))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
