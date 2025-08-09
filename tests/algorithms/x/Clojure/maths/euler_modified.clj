(ns main (:refer-clojure :exclude [ceil_float exp_approx euler_modified f1 f2 main]))

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

(declare ceil_float exp_approx euler_modified f1 f2 main)

(def ^:dynamic ceil_float_i nil)

(def ^:dynamic euler_modified_k nil)

(def ^:dynamic euler_modified_n nil)

(def ^:dynamic euler_modified_slope1 nil)

(def ^:dynamic euler_modified_slope2 nil)

(def ^:dynamic euler_modified_x nil)

(def ^:dynamic euler_modified_y nil)

(def ^:dynamic euler_modified_y_next nil)

(def ^:dynamic euler_modified_y_predict nil)

(def ^:dynamic exp_approx_n nil)

(def ^:dynamic exp_approx_sum nil)

(def ^:dynamic exp_approx_term nil)

(def ^:dynamic main_y1 nil)

(def ^:dynamic main_y2 nil)

(defn ceil_float [ceil_float_x]
  (binding [ceil_float_i nil] (try (do (set! ceil_float_i (long ceil_float_x)) (if (> ceil_float_x (double ceil_float_i)) (+ ceil_float_i 1) ceil_float_i)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn exp_approx [exp_approx_x]
  (binding [exp_approx_n nil exp_approx_sum nil exp_approx_term nil] (try (do (set! exp_approx_term 1.0) (set! exp_approx_sum 1.0) (set! exp_approx_n 1) (while (< exp_approx_n 20) (do (set! exp_approx_term (quot (* exp_approx_term exp_approx_x) (double exp_approx_n))) (set! exp_approx_sum (+ exp_approx_sum exp_approx_term)) (set! exp_approx_n (+ exp_approx_n 1)))) (throw (ex-info "return" {:v exp_approx_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn euler_modified [euler_modified_ode_func euler_modified_y0 euler_modified_x0 euler_modified_step euler_modified_x_end]
  (binding [euler_modified_k nil euler_modified_n nil euler_modified_slope1 nil euler_modified_slope2 nil euler_modified_x nil euler_modified_y nil euler_modified_y_next nil euler_modified_y_predict nil] (try (do (set! euler_modified_n (ceil_float (quot (- euler_modified_x_end euler_modified_x0) euler_modified_step))) (set! euler_modified_y [euler_modified_y0]) (set! euler_modified_x euler_modified_x0) (set! euler_modified_k 0) (while (< euler_modified_k euler_modified_n) (do (set! euler_modified_y_predict (+ (nth euler_modified_y euler_modified_k) (* euler_modified_step (euler_modified_ode_func euler_modified_x (nth euler_modified_y euler_modified_k))))) (set! euler_modified_slope1 (euler_modified_ode_func euler_modified_x (nth euler_modified_y euler_modified_k))) (set! euler_modified_slope2 (euler_modified_ode_func (+ euler_modified_x euler_modified_step) euler_modified_y_predict)) (set! euler_modified_y_next (+ (nth euler_modified_y euler_modified_k) (* (/ euler_modified_step 2.0) (+ euler_modified_slope1 euler_modified_slope2)))) (set! euler_modified_y (conj euler_modified_y euler_modified_y_next)) (set! euler_modified_x (+ euler_modified_x euler_modified_step)) (set! euler_modified_k (+ euler_modified_k 1)))) (throw (ex-info "return" {:v euler_modified_y}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn f1 [f1_x f1_y]
  (try (throw (ex-info "return" {:v (* (* (* (- 2.0) f1_x) f1_y) f1_y)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn f2 [f2_x f2_y]
  (try (throw (ex-info "return" {:v (+ (* (- 2.0) f2_y) (* (* (* f2_x f2_x) f2_x) (exp_approx (* (- 2.0) f2_x))))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (binding [main_y1 nil main_y2 nil] (do (set! main_y1 (euler_modified f1 1.0 0.0 0.2 1.0)) (println (nth main_y1 (- (count main_y1) 1))) (set! main_y2 (euler_modified f2 1.0 0.0 0.1 0.3)) (println (nth main_y2 (- (count main_y2) 1))))))

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
