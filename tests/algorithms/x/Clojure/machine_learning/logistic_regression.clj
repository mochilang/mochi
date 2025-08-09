(ns main (:refer-clojure :exclude [expApprox sigmoid dot zeros logistic_reg]))

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

(declare expApprox sigmoid dot zeros logistic_reg)

(def ^:dynamic dot_i nil)

(def ^:dynamic dot_s nil)

(def ^:dynamic expApprox_is_neg nil)

(def ^:dynamic expApprox_n nil)

(def ^:dynamic expApprox_sum nil)

(def ^:dynamic expApprox_term nil)

(def ^:dynamic expApprox_y nil)

(def ^:dynamic logistic_reg_grad nil)

(def ^:dynamic logistic_reg_h nil)

(def ^:dynamic logistic_reg_i nil)

(def ^:dynamic logistic_reg_iter nil)

(def ^:dynamic logistic_reg_k nil)

(def ^:dynamic logistic_reg_k2 nil)

(def ^:dynamic logistic_reg_m nil)

(def ^:dynamic logistic_reg_n nil)

(def ^:dynamic logistic_reg_theta nil)

(def ^:dynamic logistic_reg_z nil)

(def ^:dynamic zeros_i nil)

(def ^:dynamic zeros_res nil)

(defn expApprox [expApprox_x]
  (binding [expApprox_is_neg nil expApprox_n nil expApprox_sum nil expApprox_term nil expApprox_y nil] (try (do (set! expApprox_y expApprox_x) (set! expApprox_is_neg false) (when (< expApprox_x 0.0) (do (set! expApprox_is_neg true) (set! expApprox_y (- expApprox_x)))) (set! expApprox_term 1.0) (set! expApprox_sum 1.0) (set! expApprox_n 1) (while (< expApprox_n 30) (do (set! expApprox_term (quot (* expApprox_term expApprox_y) (double expApprox_n))) (set! expApprox_sum (+ expApprox_sum expApprox_term)) (set! expApprox_n (+ expApprox_n 1)))) (if expApprox_is_neg (/ 1.0 expApprox_sum) expApprox_sum)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sigmoid [sigmoid_z]
  (try (throw (ex-info "return" {:v (/ 1.0 (+ 1.0 (expApprox (- sigmoid_z))))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn dot [dot_a dot_b]
  (binding [dot_i nil dot_s nil] (try (do (set! dot_s 0.0) (set! dot_i 0) (while (< dot_i (count dot_a)) (do (set! dot_s (+ dot_s (* (nth dot_a dot_i) (nth dot_b dot_i)))) (set! dot_i (+ dot_i 1)))) (throw (ex-info "return" {:v dot_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn zeros [zeros_n]
  (binding [zeros_i nil zeros_res nil] (try (do (set! zeros_res []) (set! zeros_i 0) (while (< zeros_i zeros_n) (do (set! zeros_res (conj zeros_res 0.0)) (set! zeros_i (+ zeros_i 1)))) (throw (ex-info "return" {:v zeros_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn logistic_reg [logistic_reg_alpha logistic_reg_x logistic_reg_y logistic_reg_iterations]
  (binding [logistic_reg_grad nil logistic_reg_h nil logistic_reg_i nil logistic_reg_iter nil logistic_reg_k nil logistic_reg_k2 nil logistic_reg_m nil logistic_reg_n nil logistic_reg_theta nil logistic_reg_z nil] (try (do (set! logistic_reg_m (count logistic_reg_x)) (set! logistic_reg_n (count (nth logistic_reg_x 0))) (set! logistic_reg_theta (zeros logistic_reg_n)) (set! logistic_reg_iter 0) (while (< logistic_reg_iter logistic_reg_iterations) (do (set! logistic_reg_grad (zeros logistic_reg_n)) (set! logistic_reg_i 0) (while (< logistic_reg_i logistic_reg_m) (do (set! logistic_reg_z (dot (nth logistic_reg_x logistic_reg_i) logistic_reg_theta)) (set! logistic_reg_h (sigmoid logistic_reg_z)) (set! logistic_reg_k 0) (while (< logistic_reg_k logistic_reg_n) (do (set! logistic_reg_grad (assoc logistic_reg_grad logistic_reg_k (+ (nth logistic_reg_grad logistic_reg_k) (* (- logistic_reg_h (nth logistic_reg_y logistic_reg_i)) (nth (nth logistic_reg_x logistic_reg_i) logistic_reg_k))))) (set! logistic_reg_k (+ logistic_reg_k 1)))) (set! logistic_reg_i (+ logistic_reg_i 1)))) (set! logistic_reg_k2 0) (while (< logistic_reg_k2 logistic_reg_n) (do (set! logistic_reg_theta (assoc logistic_reg_theta logistic_reg_k2 (- (nth logistic_reg_theta logistic_reg_k2) (quot (* logistic_reg_alpha (nth logistic_reg_grad logistic_reg_k2)) (double logistic_reg_m))))) (set! logistic_reg_k2 (+ logistic_reg_k2 1)))) (set! logistic_reg_iter (+ logistic_reg_iter 1)))) (throw (ex-info "return" {:v logistic_reg_theta}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_x [[0.5 1.5] [1.0 1.0] [1.5 0.5] [3.0 3.5] [3.5 3.0] [4.0 4.0]])

(def ^:dynamic main_y [0.0 0.0 0.0 1.0 1.0 1.0])

(def ^:dynamic main_alpha 0.1)

(def ^:dynamic main_iterations 1000)

(def ^:dynamic main_theta (logistic_reg main_alpha main_x main_y main_iterations))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (dotimes [i (count main_theta)] (println (nth main_theta i)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
