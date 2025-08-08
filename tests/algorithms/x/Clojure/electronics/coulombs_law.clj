(ns main (:refer-clojure :exclude [abs sqrtApprox coulombs_law print_map]))

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

(declare abs sqrtApprox coulombs_law print_map)

(def ^:dynamic coulombs_law_c1 nil)

(def ^:dynamic coulombs_law_c2 nil)

(def ^:dynamic coulombs_law_charge_product nil)

(def ^:dynamic coulombs_law_d nil)

(def ^:dynamic coulombs_law_f nil)

(def ^:dynamic coulombs_law_zero_count nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(def ^:dynamic main_COULOMBS_CONSTANT 8988000000.0)

(defn abs [abs_x]
  (try (if (< abs_x 0.0) (- abs_x) abs_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (when (<= sqrtApprox_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrtApprox_guess sqrtApprox_x) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (quot sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn coulombs_law [coulombs_law_force coulombs_law_charge1 coulombs_law_charge2 coulombs_law_distance]
  (binding [coulombs_law_c1 nil coulombs_law_c2 nil coulombs_law_charge_product nil coulombs_law_d nil coulombs_law_f nil coulombs_law_zero_count nil] (try (do (set! coulombs_law_charge_product (abs (* coulombs_law_charge1 coulombs_law_charge2))) (set! coulombs_law_zero_count 0) (when (= coulombs_law_force 0.0) (set! coulombs_law_zero_count (+ coulombs_law_zero_count 1))) (when (= coulombs_law_charge1 0.0) (set! coulombs_law_zero_count (+ coulombs_law_zero_count 1))) (when (= coulombs_law_charge2 0.0) (set! coulombs_law_zero_count (+ coulombs_law_zero_count 1))) (when (= coulombs_law_distance 0.0) (set! coulombs_law_zero_count (+ coulombs_law_zero_count 1))) (when (not= coulombs_law_zero_count 1) (throw (Exception. "One and only one argument must be 0"))) (when (< coulombs_law_distance 0.0) (throw (Exception. "Distance cannot be negative"))) (when (= coulombs_law_force 0.0) (do (set! coulombs_law_f (quot (* main_COULOMBS_CONSTANT coulombs_law_charge_product) (* coulombs_law_distance coulombs_law_distance))) (throw (ex-info "return" {:v {"force" coulombs_law_f}})))) (when (= coulombs_law_charge1 0.0) (do (set! coulombs_law_c1 (quot (* (abs coulombs_law_force) (* coulombs_law_distance coulombs_law_distance)) (* main_COULOMBS_CONSTANT coulombs_law_charge2))) (throw (ex-info "return" {:v {"charge1" coulombs_law_c1}})))) (when (= coulombs_law_charge2 0.0) (do (set! coulombs_law_c2 (quot (* (abs coulombs_law_force) (* coulombs_law_distance coulombs_law_distance)) (* main_COULOMBS_CONSTANT coulombs_law_charge1))) (throw (ex-info "return" {:v {"charge2" coulombs_law_c2}})))) (set! coulombs_law_d (sqrtApprox (quot (* main_COULOMBS_CONSTANT coulombs_law_charge_product) (abs coulombs_law_force)))) (throw (ex-info "return" {:v {"distance" coulombs_law_d}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_map [print_map_m]
  (doseq [k print_map_m] (println (str (str (str (str "{\"" k) "\": ") (str (nth print_map_m k))) "}"))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (print_map (coulombs_law 0.0 3.0 5.0 2000.0))
      (print_map (coulombs_law 10.0 3.0 5.0 0.0))
      (print_map (coulombs_law 10.0 0.0 5.0 2000.0))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
