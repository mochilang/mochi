(ns main (:refer-clojure :exclude [abs_float average_absolute_deviation]))

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

(declare abs_float average_absolute_deviation)

(def ^:dynamic average_absolute_deviation_dev_sum nil)

(def ^:dynamic average_absolute_deviation_mean nil)

(def ^:dynamic average_absolute_deviation_n nil)

(def ^:dynamic average_absolute_deviation_sum nil)

(defn abs_float [abs_float_x]
  (try (if (< abs_float_x 0.0) (- abs_float_x) abs_float_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn average_absolute_deviation [average_absolute_deviation_nums]
  (binding [average_absolute_deviation_dev_sum nil average_absolute_deviation_mean nil average_absolute_deviation_n nil average_absolute_deviation_sum nil] (try (do (when (= (count average_absolute_deviation_nums) 0) (throw (Exception. "List is empty"))) (set! average_absolute_deviation_sum 0) (doseq [x average_absolute_deviation_nums] (set! average_absolute_deviation_sum (+ average_absolute_deviation_sum x))) (set! average_absolute_deviation_n (double (count average_absolute_deviation_nums))) (set! average_absolute_deviation_mean (quot (double average_absolute_deviation_sum) average_absolute_deviation_n)) (set! average_absolute_deviation_dev_sum 0.0) (doseq [x average_absolute_deviation_nums] (set! average_absolute_deviation_dev_sum (+ average_absolute_deviation_dev_sum (abs_float (- (double x) average_absolute_deviation_mean))))) (throw (ex-info "return" {:v (quot average_absolute_deviation_dev_sum average_absolute_deviation_n)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (average_absolute_deviation [0])))
      (println (str (average_absolute_deviation [4 1 3 2])))
      (println (str (average_absolute_deviation [2 70 6 50 20 8 4 0])))
      (println (str (average_absolute_deviation [(- 20) 0 30 15])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
