(ns main (:refer-clojure :exclude [to_radians sin_taylor cos_taylor exp_taylor gabor_filter_kernel]))

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

(declare to_radians sin_taylor cos_taylor exp_taylor gabor_filter_kernel)

(def ^:dynamic cos_taylor_i nil)

(def ^:dynamic cos_taylor_k1 nil)

(def ^:dynamic cos_taylor_k2 nil)

(def ^:dynamic cos_taylor_sum nil)

(def ^:dynamic cos_taylor_term nil)

(def ^:dynamic exp_taylor_i nil)

(def ^:dynamic exp_taylor_sum nil)

(def ^:dynamic exp_taylor_term nil)

(def ^:dynamic gabor_filter_kernel_cos_theta nil)

(def ^:dynamic gabor_filter_kernel_exponent nil)

(def ^:dynamic gabor_filter_kernel_gabor nil)

(def ^:dynamic gabor_filter_kernel_px nil)

(def ^:dynamic gabor_filter_kernel_py nil)

(def ^:dynamic gabor_filter_kernel_rad nil)

(def ^:dynamic gabor_filter_kernel_row nil)

(def ^:dynamic gabor_filter_kernel_sin_theta nil)

(def ^:dynamic gabor_filter_kernel_size nil)

(def ^:dynamic gabor_filter_kernel_value nil)

(def ^:dynamic gabor_filter_kernel_x nil)

(def ^:dynamic gabor_filter_kernel_x_rot nil)

(def ^:dynamic gabor_filter_kernel_y nil)

(def ^:dynamic gabor_filter_kernel_y_rot nil)

(def ^:dynamic sin_taylor_i nil)

(def ^:dynamic sin_taylor_k1 nil)

(def ^:dynamic sin_taylor_k2 nil)

(def ^:dynamic sin_taylor_sum nil)

(def ^:dynamic sin_taylor_term nil)

(def ^:dynamic main_PI 3.141592653589793)

(defn to_radians [to_radians_deg]
  (try (throw (ex-info "return" {:v (/ (* to_radians_deg main_PI) 180.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sin_taylor [sin_taylor_x]
  (binding [sin_taylor_i nil sin_taylor_k1 nil sin_taylor_k2 nil sin_taylor_sum nil sin_taylor_term nil] (try (do (set! sin_taylor_term sin_taylor_x) (set! sin_taylor_sum sin_taylor_x) (set! sin_taylor_i 1) (while (< sin_taylor_i 10) (do (set! sin_taylor_k1 (* 2.0 (double sin_taylor_i))) (set! sin_taylor_k2 (+ sin_taylor_k1 1.0)) (set! sin_taylor_term (quot (* (* (- sin_taylor_term) sin_taylor_x) sin_taylor_x) (* sin_taylor_k1 sin_taylor_k2))) (set! sin_taylor_sum (+ sin_taylor_sum sin_taylor_term)) (set! sin_taylor_i (+ sin_taylor_i 1)))) (throw (ex-info "return" {:v sin_taylor_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn cos_taylor [cos_taylor_x]
  (binding [cos_taylor_i nil cos_taylor_k1 nil cos_taylor_k2 nil cos_taylor_sum nil cos_taylor_term nil] (try (do (set! cos_taylor_term 1.0) (set! cos_taylor_sum 1.0) (set! cos_taylor_i 1) (while (< cos_taylor_i 10) (do (set! cos_taylor_k1 (- (* 2.0 (double cos_taylor_i)) 1.0)) (set! cos_taylor_k2 (* 2.0 (double cos_taylor_i))) (set! cos_taylor_term (quot (* (* (- cos_taylor_term) cos_taylor_x) cos_taylor_x) (* cos_taylor_k1 cos_taylor_k2))) (set! cos_taylor_sum (+ cos_taylor_sum cos_taylor_term)) (set! cos_taylor_i (+ cos_taylor_i 1)))) (throw (ex-info "return" {:v cos_taylor_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn exp_taylor [exp_taylor_x]
  (binding [exp_taylor_i nil exp_taylor_sum nil exp_taylor_term nil] (try (do (set! exp_taylor_term 1.0) (set! exp_taylor_sum 1.0) (set! exp_taylor_i 1.0) (while (< exp_taylor_i 20.0) (do (set! exp_taylor_term (quot (* exp_taylor_term exp_taylor_x) exp_taylor_i)) (set! exp_taylor_sum (+ exp_taylor_sum exp_taylor_term)) (set! exp_taylor_i (+ exp_taylor_i 1.0)))) (throw (ex-info "return" {:v exp_taylor_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn gabor_filter_kernel [gabor_filter_kernel_ksize gabor_filter_kernel_sigma gabor_filter_kernel_theta gabor_filter_kernel_lambd gabor_filter_kernel_gamma gabor_filter_kernel_psi]
  (binding [gabor_filter_kernel_cos_theta nil gabor_filter_kernel_exponent nil gabor_filter_kernel_gabor nil gabor_filter_kernel_px nil gabor_filter_kernel_py nil gabor_filter_kernel_rad nil gabor_filter_kernel_row nil gabor_filter_kernel_sin_theta nil gabor_filter_kernel_size nil gabor_filter_kernel_value nil gabor_filter_kernel_x nil gabor_filter_kernel_x_rot nil gabor_filter_kernel_y nil gabor_filter_kernel_y_rot nil] (try (do (set! gabor_filter_kernel_size gabor_filter_kernel_ksize) (when (= (mod gabor_filter_kernel_size 2) 0) (set! gabor_filter_kernel_size (+ gabor_filter_kernel_size 1))) (set! gabor_filter_kernel_gabor []) (set! gabor_filter_kernel_y 0) (while (< gabor_filter_kernel_y gabor_filter_kernel_size) (do (set! gabor_filter_kernel_row []) (set! gabor_filter_kernel_x 0) (while (< gabor_filter_kernel_x gabor_filter_kernel_size) (do (set! gabor_filter_kernel_px (double (- gabor_filter_kernel_x (quot gabor_filter_kernel_size 2)))) (set! gabor_filter_kernel_py (double (- gabor_filter_kernel_y (quot gabor_filter_kernel_size 2)))) (set! gabor_filter_kernel_rad (to_radians gabor_filter_kernel_theta)) (set! gabor_filter_kernel_cos_theta (cos_taylor gabor_filter_kernel_rad)) (set! gabor_filter_kernel_sin_theta (sin_taylor gabor_filter_kernel_rad)) (set! gabor_filter_kernel_x_rot (+ (* gabor_filter_kernel_cos_theta gabor_filter_kernel_px) (* gabor_filter_kernel_sin_theta gabor_filter_kernel_py))) (set! gabor_filter_kernel_y_rot (+ (* (- gabor_filter_kernel_sin_theta) gabor_filter_kernel_px) (* gabor_filter_kernel_cos_theta gabor_filter_kernel_py))) (set! gabor_filter_kernel_exponent (quot (- (+ (* gabor_filter_kernel_x_rot gabor_filter_kernel_x_rot) (* (* (* gabor_filter_kernel_gamma gabor_filter_kernel_gamma) gabor_filter_kernel_y_rot) gabor_filter_kernel_y_rot))) (* (* 2.0 gabor_filter_kernel_sigma) gabor_filter_kernel_sigma))) (set! gabor_filter_kernel_value (* (exp_taylor gabor_filter_kernel_exponent) (cos_taylor (+ (quot (* (* 2.0 main_PI) gabor_filter_kernel_x_rot) gabor_filter_kernel_lambd) gabor_filter_kernel_psi)))) (set! gabor_filter_kernel_row (conj gabor_filter_kernel_row gabor_filter_kernel_value)) (set! gabor_filter_kernel_x (+ gabor_filter_kernel_x 1)))) (set! gabor_filter_kernel_gabor (conj gabor_filter_kernel_gabor gabor_filter_kernel_row)) (set! gabor_filter_kernel_y (+ gabor_filter_kernel_y 1)))) (throw (ex-info "return" {:v gabor_filter_kernel_gabor}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_kernel (gabor_filter_kernel 3 8.0 0.0 10.0 0.0 0.0))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println main_kernel)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
