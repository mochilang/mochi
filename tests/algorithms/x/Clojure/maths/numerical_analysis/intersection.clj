(ns main (:refer-clojure :exclude [abs_float intersection f main]))

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

(declare abs_float intersection f main)

(def ^:dynamic intersection_denominator nil)

(def ^:dynamic intersection_numerator nil)

(def ^:dynamic intersection_x_n nil)

(def ^:dynamic intersection_x_n1 nil)

(def ^:dynamic intersection_x_n2 nil)

(defn abs_float [abs_float_x]
  (try (if (< abs_float_x 0.0) (- abs_float_x) abs_float_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn intersection [intersection_function intersection_x0 intersection_x1]
  (binding [intersection_denominator nil intersection_numerator nil intersection_x_n nil intersection_x_n1 nil intersection_x_n2 nil] (try (do (set! intersection_x_n intersection_x0) (set! intersection_x_n1 intersection_x1) (while true (do (when (or (= intersection_x_n intersection_x_n1) (= (intersection_function intersection_x_n1) (intersection_function intersection_x_n))) (throw (Exception. "float division by zero, could not find root"))) (set! intersection_numerator (intersection_function intersection_x_n1)) (set! intersection_denominator (quot (- (intersection_function intersection_x_n1) (intersection_function intersection_x_n)) (- intersection_x_n1 intersection_x_n))) (set! intersection_x_n2 (- intersection_x_n1 (quot intersection_numerator intersection_denominator))) (when (< (abs_float (- intersection_x_n2 intersection_x_n1)) 0.00001) (throw (ex-info "return" {:v intersection_x_n2}))) (set! intersection_x_n intersection_x_n1) (set! intersection_x_n1 intersection_x_n2)))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn f [f_x]
  (try (throw (ex-info "return" {:v (- (- (* (* f_x f_x) f_x) (* 2.0 f_x)) 5.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (println (str (intersection f 3.0 3.5))))

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
