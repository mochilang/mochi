(ns main (:refer-clojure :exclude [n_choose_k pow_float basis_function bezier_point]))

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

(declare n_choose_k pow_float basis_function bezier_point)

(def ^:dynamic basis_function_coef nil)

(def ^:dynamic basis_function_degree nil)

(def ^:dynamic basis_function_i nil)

(def ^:dynamic basis_function_res nil)

(def ^:dynamic basis_function_term nil)

(def ^:dynamic bezier_point_basis nil)

(def ^:dynamic bezier_point_i nil)

(def ^:dynamic bezier_point_x nil)

(def ^:dynamic bezier_point_y nil)

(def ^:dynamic n_choose_k_i nil)

(def ^:dynamic n_choose_k_result nil)

(def ^:dynamic pow_float_i nil)

(def ^:dynamic pow_float_result nil)

(defn n_choose_k [n_choose_k_n n_choose_k_k]
  (binding [n_choose_k_i nil n_choose_k_result nil] (try (do (when (or (< n_choose_k_k 0) (> n_choose_k_k n_choose_k_n)) (throw (ex-info "return" {:v 0.0}))) (when (or (= n_choose_k_k 0) (= n_choose_k_k n_choose_k_n)) (throw (ex-info "return" {:v 1.0}))) (set! n_choose_k_result 1.0) (set! n_choose_k_i 1) (while (<= n_choose_k_i n_choose_k_k) (do (set! n_choose_k_result (quot (* n_choose_k_result (* 1.0 (+ (- n_choose_k_n n_choose_k_k) n_choose_k_i))) (* 1.0 n_choose_k_i))) (set! n_choose_k_i (+ n_choose_k_i 1)))) (throw (ex-info "return" {:v n_choose_k_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow_float [pow_float_base pow_float_exp]
  (binding [pow_float_i nil pow_float_result nil] (try (do (set! pow_float_result 1.0) (set! pow_float_i 0) (while (< pow_float_i pow_float_exp) (do (set! pow_float_result (* pow_float_result pow_float_base)) (set! pow_float_i (+ pow_float_i 1)))) (throw (ex-info "return" {:v pow_float_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn basis_function [basis_function_points basis_function_t]
  (binding [basis_function_coef nil basis_function_degree nil basis_function_i nil basis_function_res nil basis_function_term nil] (try (do (set! basis_function_degree (- (count basis_function_points) 1)) (set! basis_function_res []) (set! basis_function_i 0) (while (<= basis_function_i basis_function_degree) (do (set! basis_function_coef (n_choose_k basis_function_degree basis_function_i)) (set! basis_function_term (* (pow_float (- 1.0 basis_function_t) (- basis_function_degree basis_function_i)) (pow_float basis_function_t basis_function_i))) (set! basis_function_res (conj basis_function_res (* basis_function_coef basis_function_term))) (set! basis_function_i (+ basis_function_i 1)))) (throw (ex-info "return" {:v basis_function_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bezier_point [bezier_point_points bezier_point_t]
  (binding [bezier_point_basis nil bezier_point_i nil bezier_point_x nil bezier_point_y nil] (try (do (set! bezier_point_basis (basis_function bezier_point_points bezier_point_t)) (set! bezier_point_x 0.0) (set! bezier_point_y 0.0) (set! bezier_point_i 0) (while (< bezier_point_i (count bezier_point_points)) (do (set! bezier_point_x (+ bezier_point_x (* (nth bezier_point_basis bezier_point_i) (nth (nth bezier_point_points bezier_point_i) 0)))) (set! bezier_point_y (+ bezier_point_y (* (nth bezier_point_basis bezier_point_i) (nth (nth bezier_point_points bezier_point_i) 1)))) (set! bezier_point_i (+ bezier_point_i 1)))) (throw (ex-info "return" {:v [bezier_point_x bezier_point_y]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_control [[1.0 1.0] [1.0 2.0]])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (basis_function main_control 0.0)))
      (println (str (basis_function main_control 1.0)))
      (println (str (bezier_point main_control 0.0)))
      (println (str (bezier_point main_control 1.0)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
