(ns main (:refer-clojure :exclude [dual pow_float add sub mul div power main]))

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

(declare dual pow_float add sub mul div power main)

(def ^:dynamic main_a nil)

(def ^:dynamic main_b nil)

(def ^:dynamic main_c nil)

(def ^:dynamic main_d nil)

(def ^:dynamic main_e nil)

(def ^:dynamic main_x nil)

(def ^:dynamic main_y nil)

(def ^:dynamic pow_float_i nil)

(def ^:dynamic pow_float_res nil)

(defn dual [dual_v dual_d]
  (try (throw (ex-info "return" {:v {:deriv dual_d :value dual_v}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pow_float [pow_float_base pow_float_exp]
  (binding [pow_float_i nil pow_float_res nil] (try (do (set! pow_float_res 1.0) (set! pow_float_i 0) (while (< pow_float_i pow_float_exp) (do (set! pow_float_res (* pow_float_res pow_float_base)) (set! pow_float_i (+ pow_float_i 1)))) (throw (ex-info "return" {:v pow_float_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn add [add_a add_b]
  (try (throw (ex-info "return" {:v {:deriv (+ (:deriv add_a) (:deriv add_b)) :value (+ (:value add_a) (:value add_b))}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sub [sub_a sub_b]
  (try (throw (ex-info "return" {:v {:deriv (- (:deriv sub_a) (:deriv sub_b)) :value (- (:value sub_a) (:value sub_b))}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn mul [mul_a mul_b]
  (try (throw (ex-info "return" {:v {:deriv (+ (* (:deriv mul_a) (:value mul_b)) (* (:deriv mul_b) (:value mul_a))) :value (* (:value mul_a) (:value mul_b))}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn div [div_a div_b]
  (try (throw (ex-info "return" {:v {:deriv (quot (- (* (:deriv div_a) (:value div_b)) (* (:deriv div_b) (:value div_a))) (* (:value div_b) (:value div_b))) :value (quot (:value div_a) (:value div_b))}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn power [power_a power_p]
  (try (throw (ex-info "return" {:v {:deriv (* (* (* 1.0 power_p) (pow_float (:value power_a) (- power_p 1))) (:deriv power_a)) :value (pow_float (:value power_a) power_p)}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (binding [main_a nil main_b nil main_c nil main_d nil main_e nil main_x nil main_y nil] (do (set! main_a (dual 2.0 1.0)) (set! main_b (dual 1.0 0.0)) (set! main_c (add main_a main_b)) (set! main_d (mul main_a main_b)) (set! main_e (div main_c main_d)) (println (str (:deriv main_e))) (set! main_x (dual 2.0 1.0)) (set! main_y (power main_x 3)) (println (str (:deriv main_y))))))

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
