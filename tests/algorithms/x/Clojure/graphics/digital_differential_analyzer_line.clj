(ns main (:refer-clojure :exclude [abs_int round_int digital_differential_analyzer_line main]))

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

(declare abs_int round_int digital_differential_analyzer_line main)

(def ^:dynamic digital_differential_analyzer_line_abs_dx nil)

(def ^:dynamic digital_differential_analyzer_line_abs_dy nil)

(def ^:dynamic digital_differential_analyzer_line_coordinates nil)

(def ^:dynamic digital_differential_analyzer_line_dx nil)

(def ^:dynamic digital_differential_analyzer_line_dy nil)

(def ^:dynamic digital_differential_analyzer_line_i nil)

(def ^:dynamic digital_differential_analyzer_line_point nil)

(def ^:dynamic digital_differential_analyzer_line_steps nil)

(def ^:dynamic digital_differential_analyzer_line_x nil)

(def ^:dynamic digital_differential_analyzer_line_x_increment nil)

(def ^:dynamic digital_differential_analyzer_line_y nil)

(def ^:dynamic digital_differential_analyzer_line_y_increment nil)

(def ^:dynamic main_result nil)

(defn abs_int [abs_int_n]
  (try (if (< abs_int_n 0) (- abs_int_n) abs_int_n) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn round_int [round_int_x]
  (try (throw (ex-info "return" {:v (long (+ round_int_x 0.5))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn digital_differential_analyzer_line [digital_differential_analyzer_line_p1 digital_differential_analyzer_line_p2]
  (binding [digital_differential_analyzer_line_abs_dx nil digital_differential_analyzer_line_abs_dy nil digital_differential_analyzer_line_coordinates nil digital_differential_analyzer_line_dx nil digital_differential_analyzer_line_dy nil digital_differential_analyzer_line_i nil digital_differential_analyzer_line_point nil digital_differential_analyzer_line_steps nil digital_differential_analyzer_line_x nil digital_differential_analyzer_line_x_increment nil digital_differential_analyzer_line_y nil digital_differential_analyzer_line_y_increment nil] (try (do (set! digital_differential_analyzer_line_dx (- (:x digital_differential_analyzer_line_p2) (:x digital_differential_analyzer_line_p1))) (set! digital_differential_analyzer_line_dy (- (:y digital_differential_analyzer_line_p2) (:y digital_differential_analyzer_line_p1))) (set! digital_differential_analyzer_line_abs_dx (abs_int digital_differential_analyzer_line_dx)) (set! digital_differential_analyzer_line_abs_dy (abs_int digital_differential_analyzer_line_dy)) (set! digital_differential_analyzer_line_steps (if (> digital_differential_analyzer_line_abs_dx digital_differential_analyzer_line_abs_dy) digital_differential_analyzer_line_abs_dx digital_differential_analyzer_line_abs_dy)) (set! digital_differential_analyzer_line_x_increment (quot (double digital_differential_analyzer_line_dx) (double digital_differential_analyzer_line_steps))) (set! digital_differential_analyzer_line_y_increment (quot (double digital_differential_analyzer_line_dy) (double digital_differential_analyzer_line_steps))) (set! digital_differential_analyzer_line_coordinates []) (set! digital_differential_analyzer_line_x (double (:x digital_differential_analyzer_line_p1))) (set! digital_differential_analyzer_line_y (double (:y digital_differential_analyzer_line_p1))) (set! digital_differential_analyzer_line_i 0) (while (< digital_differential_analyzer_line_i digital_differential_analyzer_line_steps) (do (set! digital_differential_analyzer_line_x (+ digital_differential_analyzer_line_x digital_differential_analyzer_line_x_increment)) (set! digital_differential_analyzer_line_y (+ digital_differential_analyzer_line_y digital_differential_analyzer_line_y_increment)) (set! digital_differential_analyzer_line_point {:x (round_int digital_differential_analyzer_line_x) :y (round_int digital_differential_analyzer_line_y)}) (set! digital_differential_analyzer_line_coordinates (conj digital_differential_analyzer_line_coordinates digital_differential_analyzer_line_point)) (set! digital_differential_analyzer_line_i (+ digital_differential_analyzer_line_i 1)))) (throw (ex-info "return" {:v digital_differential_analyzer_line_coordinates}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_result nil] (do (set! main_result (digital_differential_analyzer_line {:x 1 :y 1} {:x 4 :y 4})) (println main_result))))

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
