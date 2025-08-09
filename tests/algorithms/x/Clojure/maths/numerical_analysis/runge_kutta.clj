(ns main (:refer-clojure :exclude [runge_kutta test_runge_kutta main]))

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

(declare runge_kutta test_runge_kutta main)

(def ^:dynamic main_r nil)

(def ^:dynamic runge_kutta_i nil)

(def ^:dynamic runge_kutta_k nil)

(def ^:dynamic runge_kutta_k1 nil)

(def ^:dynamic runge_kutta_k2 nil)

(def ^:dynamic runge_kutta_k3 nil)

(def ^:dynamic runge_kutta_k4 nil)

(def ^:dynamic runge_kutta_n nil)

(def ^:dynamic runge_kutta_span nil)

(def ^:dynamic runge_kutta_x nil)

(def ^:dynamic runge_kutta_y nil)

(def ^:dynamic test_runge_kutta_diff nil)

(def ^:dynamic test_runge_kutta_expected nil)

(def ^:dynamic test_runge_kutta_last nil)

(def ^:dynamic test_runge_kutta_result nil)

(defn runge_kutta [runge_kutta_f runge_kutta_y0 runge_kutta_x0 runge_kutta_h runge_kutta_x_end]
  (binding [runge_kutta_i nil runge_kutta_k nil runge_kutta_k1 nil runge_kutta_k2 nil runge_kutta_k3 nil runge_kutta_k4 nil runge_kutta_n nil runge_kutta_span nil runge_kutta_x nil runge_kutta_y nil] (try (do (set! runge_kutta_span (quot (- runge_kutta_x_end runge_kutta_x0) runge_kutta_h)) (set! runge_kutta_n (int runge_kutta_span)) (when (< (float runge_kutta_n) runge_kutta_span) (set! runge_kutta_n (+ runge_kutta_n 1))) (set! runge_kutta_y []) (set! runge_kutta_i 0) (while (< runge_kutta_i (+ runge_kutta_n 1)) (do (set! runge_kutta_y (conj runge_kutta_y 0.0)) (set! runge_kutta_i (+ runge_kutta_i 1)))) (set! runge_kutta_y (assoc runge_kutta_y 0 runge_kutta_y0)) (set! runge_kutta_x runge_kutta_x0) (set! runge_kutta_k 0) (while (< runge_kutta_k runge_kutta_n) (do (set! runge_kutta_k1 (runge_kutta_f runge_kutta_x (nth runge_kutta_y runge_kutta_k))) (set! runge_kutta_k2 (runge_kutta_f (+ runge_kutta_x (* 0.5 runge_kutta_h)) (+ (nth runge_kutta_y runge_kutta_k) (* (* 0.5 runge_kutta_h) runge_kutta_k1)))) (set! runge_kutta_k3 (runge_kutta_f (+ runge_kutta_x (* 0.5 runge_kutta_h)) (+ (nth runge_kutta_y runge_kutta_k) (* (* 0.5 runge_kutta_h) runge_kutta_k2)))) (set! runge_kutta_k4 (runge_kutta_f (+ runge_kutta_x runge_kutta_h) (+ (nth runge_kutta_y runge_kutta_k) (* runge_kutta_h runge_kutta_k3)))) (set! runge_kutta_y (assoc runge_kutta_y (+ runge_kutta_k 1) (+ (nth runge_kutta_y runge_kutta_k) (* (* (/ 1.0 6.0) runge_kutta_h) (+ (+ (+ runge_kutta_k1 (* 2.0 runge_kutta_k2)) (* 2.0 runge_kutta_k3)) runge_kutta_k4))))) (set! runge_kutta_x (+ runge_kutta_x runge_kutta_h)) (set! runge_kutta_k (+ runge_kutta_k 1)))) (throw (ex-info "return" {:v runge_kutta_y}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn f [f_x f_y]
  (try (throw (ex-info "return" {:v f_y})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn test_runge_kutta []
  (binding [test_runge_kutta_diff nil test_runge_kutta_expected nil test_runge_kutta_last nil test_runge_kutta_result nil] (try (do (set! test_runge_kutta_result (runge_kutta f 1.0 0.0 0.01 5.0)) (set! test_runge_kutta_last (nth test_runge_kutta_result (- (count test_runge_kutta_result) 1))) (set! test_runge_kutta_expected 148.41315904125113) (set! test_runge_kutta_diff (- test_runge_kutta_last test_runge_kutta_expected)) (when (< test_runge_kutta_diff 0.0) (set! test_runge_kutta_diff (- test_runge_kutta_diff))) (when (> test_runge_kutta_diff 0.000001) (throw (Exception. "runge_kutta failed")))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn f [f_x f_y]
  (try (throw (ex-info "return" {:v f_y})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (binding [main_r nil] (try (do (test_runge_kutta) (set! main_r (runge_kutta f 1.0 0.0 0.1 1.0)) (println (str (nth main_r (- (count main_r) 1))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

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
