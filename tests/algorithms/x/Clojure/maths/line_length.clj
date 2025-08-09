(ns main (:refer-clojure :exclude [sqrt_newton hypot line_length f1 f2 f3]))

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

(declare sqrt_newton hypot line_length f1 f2 f3)

(def ^:dynamic line_length_fx1 nil)

(def ^:dynamic line_length_fx2 nil)

(def ^:dynamic line_length_i nil)

(def ^:dynamic line_length_length nil)

(def ^:dynamic line_length_step nil)

(def ^:dynamic line_length_x1 nil)

(def ^:dynamic line_length_x2 nil)

(def ^:dynamic sqrt_newton_i nil)

(def ^:dynamic sqrt_newton_x nil)

(defn sqrt_newton [sqrt_newton_n]
  (binding [sqrt_newton_i nil sqrt_newton_x nil] (try (do (when (= sqrt_newton_n 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrt_newton_x sqrt_newton_n) (set! sqrt_newton_i 0) (while (< sqrt_newton_i 20) (do (set! sqrt_newton_x (/ (+ sqrt_newton_x (quot sqrt_newton_n sqrt_newton_x)) 2.0)) (set! sqrt_newton_i (+ sqrt_newton_i 1)))) (throw (ex-info "return" {:v sqrt_newton_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn hypot [hypot_a hypot_b]
  (try (throw (ex-info "return" {:v (sqrt_newton (+ (* hypot_a hypot_a) (* hypot_b hypot_b)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn line_length [line_length_fnc line_length_x_start line_length_x_end line_length_steps]
  (binding [line_length_fx1 nil line_length_fx2 nil line_length_i nil line_length_length nil line_length_step nil line_length_x1 nil line_length_x2 nil] (try (do (set! line_length_x1 line_length_x_start) (set! line_length_fx1 (line_length_fnc line_length_x_start)) (set! line_length_length 0.0) (set! line_length_i 0) (set! line_length_step (/ (- line_length_x_end line_length_x_start) (* 1.0 line_length_steps))) (while (< line_length_i line_length_steps) (do (set! line_length_x2 (+ line_length_step line_length_x1)) (set! line_length_fx2 (line_length_fnc line_length_x2)) (set! line_length_length (+ line_length_length (hypot (- line_length_x2 line_length_x1) (- line_length_fx2 line_length_fx1)))) (set! line_length_x1 line_length_x2) (set! line_length_fx1 line_length_fx2) (set! line_length_i (+ line_length_i 1)))) (throw (ex-info "return" {:v line_length_length}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn f1 [f1_x]
  (try (throw (ex-info "return" {:v f1_x})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn f2 [f2_x]
  (try (throw (ex-info "return" {:v 1.0})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn f3 [f3_x]
  (try (throw (ex-info "return" {:v (/ (* f3_x f3_x) 10.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (line_length f1 0.0 1.0 10))
      (println (line_length f2 (- 5.5) 4.5 100))
      (println (line_length f3 0.0 10.0 1000))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
