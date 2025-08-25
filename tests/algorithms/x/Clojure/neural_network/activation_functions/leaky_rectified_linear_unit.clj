(ns main (:refer-clojure :exclude [leaky_rectified_linear_unit]))

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
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare leaky_rectified_linear_unit)

(declare _read_file)

(def ^:dynamic leaky_rectified_linear_unit_i nil)

(def ^:dynamic leaky_rectified_linear_unit_result nil)

(def ^:dynamic leaky_rectified_linear_unit_x nil)

(defn leaky_rectified_linear_unit [leaky_rectified_linear_unit_vector leaky_rectified_linear_unit_alpha]
  (binding [leaky_rectified_linear_unit_i nil leaky_rectified_linear_unit_result nil leaky_rectified_linear_unit_x nil] (try (do (set! leaky_rectified_linear_unit_result []) (set! leaky_rectified_linear_unit_i 0) (while (< leaky_rectified_linear_unit_i (count leaky_rectified_linear_unit_vector)) (do (set! leaky_rectified_linear_unit_x (nth leaky_rectified_linear_unit_vector leaky_rectified_linear_unit_i)) (if (> leaky_rectified_linear_unit_x 0.0) (set! leaky_rectified_linear_unit_result (conj leaky_rectified_linear_unit_result leaky_rectified_linear_unit_x)) (set! leaky_rectified_linear_unit_result (conj leaky_rectified_linear_unit_result (*' leaky_rectified_linear_unit_alpha leaky_rectified_linear_unit_x)))) (set! leaky_rectified_linear_unit_i (+' leaky_rectified_linear_unit_i 1)))) (throw (ex-info "return" {:v leaky_rectified_linear_unit_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_vector1 nil)

(def ^:dynamic main_result1 nil)

(def ^:dynamic main_vector2 nil)

(def ^:dynamic main_result2 nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_vector1) (constantly [2.3 0.6 (- 2.0) (- 3.8)]))
      (alter-var-root (var main_result1) (constantly (leaky_rectified_linear_unit main_vector1 0.3)))
      (println (mochi_str main_result1))
      (alter-var-root (var main_vector2) (constantly [(- 9.2) (- 0.3) 0.45 (- 4.56)]))
      (alter-var-root (var main_result2) (constantly (leaky_rectified_linear_unit main_vector2 0.067)))
      (println (mochi_str main_result2))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
