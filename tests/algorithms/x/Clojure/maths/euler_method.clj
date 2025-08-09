(ns main (:refer-clojure :exclude [ceil_int explicit_euler abs_float test_explicit_euler main]))

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

(declare ceil_int explicit_euler abs_float test_explicit_euler main)

(def ^:dynamic ceil_int_n nil)

(def ^:dynamic explicit_euler_i nil)

(def ^:dynamic explicit_euler_k nil)

(def ^:dynamic explicit_euler_n nil)

(def ^:dynamic explicit_euler_x nil)

(def ^:dynamic explicit_euler_y nil)

(def ^:dynamic main_f nil)

(def ^:dynamic main_ys nil)

(def ^:dynamic test_explicit_euler_f nil)

(def ^:dynamic test_explicit_euler_last nil)

(def ^:dynamic test_explicit_euler_ys nil)

(defn ceil_int [ceil_int_x]
  (binding [ceil_int_n nil] (try (do (set! ceil_int_n (long ceil_int_x)) (when (< (float ceil_int_n) ceil_int_x) (set! ceil_int_n (+ ceil_int_n 1))) (throw (ex-info "return" {:v ceil_int_n}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn explicit_euler [explicit_euler_ode_func explicit_euler_y0 explicit_euler_x0 explicit_euler_step_size explicit_euler_x_end]
  (binding [explicit_euler_i nil explicit_euler_k nil explicit_euler_n nil explicit_euler_x nil explicit_euler_y nil] (try (do (set! explicit_euler_n (ceil_int (quot (- explicit_euler_x_end explicit_euler_x0) explicit_euler_step_size))) (set! explicit_euler_y []) (set! explicit_euler_i 0) (while (<= explicit_euler_i explicit_euler_n) (do (set! explicit_euler_y (conj explicit_euler_y 0.0)) (set! explicit_euler_i (+ explicit_euler_i 1)))) (set! explicit_euler_y (assoc explicit_euler_y 0 explicit_euler_y0)) (set! explicit_euler_x explicit_euler_x0) (set! explicit_euler_k 0) (while (< explicit_euler_k explicit_euler_n) (do (set! explicit_euler_y (assoc explicit_euler_y (+ explicit_euler_k 1) (+ (nth explicit_euler_y explicit_euler_k) (* explicit_euler_step_size (explicit_euler_ode_func explicit_euler_x (nth explicit_euler_y explicit_euler_k)))))) (set! explicit_euler_x (+ explicit_euler_x explicit_euler_step_size)) (set! explicit_euler_k (+ explicit_euler_k 1)))) (throw (ex-info "return" {:v explicit_euler_y}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn abs_float [abs_float_a]
  (try (if (< abs_float_a 0.0) (- abs_float_a) abs_float_a) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn test_explicit_euler []
  (binding [test_explicit_euler_f nil test_explicit_euler_last nil test_explicit_euler_ys nil] (do (set! test_explicit_euler_f (fn [x y] y)) (set! test_explicit_euler_ys (explicit_euler test_explicit_euler_f 1.0 0.0 0.01 5.0)) (set! test_explicit_euler_last (nth test_explicit_euler_ys (- (count test_explicit_euler_ys) 1))) (when (> (abs_float (- test_explicit_euler_last 144.77277243257308)) 0.001) (throw (Exception. "explicit_euler failed"))))))

(defn main []
  (binding [main_f nil main_ys nil] (do (test_explicit_euler) (set! main_f (fn [x y] y)) (set! main_ys (explicit_euler main_f 1.0 0.0 0.01 5.0)) (println (nth main_ys (- (count main_ys) 1))))))

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
