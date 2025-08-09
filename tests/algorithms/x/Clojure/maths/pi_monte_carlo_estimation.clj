(ns main (:refer-clojure :exclude [next_seed rand_unit is_in_unit_circle random_unit_square estimate_pi abs_float main]))

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

(declare next_seed rand_unit is_in_unit_circle random_unit_square estimate_pi abs_float main)

(def ^:dynamic estimate_pi_i nil)

(def ^:dynamic estimate_pi_inside nil)

(def ^:dynamic estimate_pi_p nil)

(def ^:dynamic main_error nil)

(def ^:dynamic main_my_pi nil)

(def ^:dynamic main_n nil)

(def ^:dynamic main_PI 3.141592653589793)

(def ^:dynamic main_seed 1)

(defn next_seed [next_seed_x]
  (try (throw (ex-info "return" {:v (mod (+ (* next_seed_x 1103515245) 12345) 2147483648)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn rand_unit []
  (try (do (alter-var-root (var main_seed) (fn [_] (next_seed main_seed))) (throw (ex-info "return" {:v (/ (double main_seed) 2147483648.0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn is_in_unit_circle [is_in_unit_circle_p]
  (try (throw (ex-info "return" {:v (<= (+ (* (:x is_in_unit_circle_p) (:x is_in_unit_circle_p)) (* (:y is_in_unit_circle_p) (:y is_in_unit_circle_p))) 1.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn random_unit_square []
  (try (throw (ex-info "return" {:v {:x (rand_unit) :y (rand_unit)}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn estimate_pi [estimate_pi_simulations]
  (binding [estimate_pi_i nil estimate_pi_inside nil estimate_pi_p nil] (try (do (when (< estimate_pi_simulations 1) (throw (Exception. "At least one simulation is necessary to estimate PI."))) (set! estimate_pi_inside 0) (set! estimate_pi_i 0) (while (< estimate_pi_i estimate_pi_simulations) (do (set! estimate_pi_p (random_unit_square)) (when (is_in_unit_circle estimate_pi_p) (set! estimate_pi_inside (+ estimate_pi_inside 1))) (set! estimate_pi_i (+ estimate_pi_i 1)))) (throw (ex-info "return" {:v (/ (* 4.0 (double estimate_pi_inside)) (double estimate_pi_simulations))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn abs_float [abs_float_x]
  (try (if (< abs_float_x 0.0) (- abs_float_x) abs_float_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (binding [main_error nil main_my_pi nil main_n nil] (do (set! main_n 10000) (set! main_my_pi (estimate_pi main_n)) (set! main_error (abs_float (- main_my_pi main_PI))) (println (str (str (str "An estimate of PI is " (str main_my_pi)) " with an error of ") (str main_error))))))

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
