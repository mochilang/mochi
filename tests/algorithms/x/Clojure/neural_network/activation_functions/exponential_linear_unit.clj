(ns main (:refer-clojure :exclude [exp_approx exponential_linear_unit]))

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

(declare exp_approx exponential_linear_unit)

(declare _read_file)

(def ^:dynamic exp_approx_absx nil)

(def ^:dynamic exp_approx_i nil)

(def ^:dynamic exp_approx_sum nil)

(def ^:dynamic exp_approx_term nil)

(def ^:dynamic exponential_linear_unit_i nil)

(def ^:dynamic exponential_linear_unit_neg nil)

(def ^:dynamic exponential_linear_unit_result nil)

(def ^:dynamic exponential_linear_unit_v nil)

(defn exp_approx [exp_approx_x]
  (binding [exp_approx_absx nil exp_approx_i nil exp_approx_sum nil exp_approx_term nil] (try (do (set! exp_approx_sum 1.0) (set! exp_approx_term 1.0) (set! exp_approx_i 1) (set! exp_approx_absx (if (< exp_approx_x 0.0) (- exp_approx_x) exp_approx_x)) (while (<= exp_approx_i 20) (do (set! exp_approx_term (/ (*' exp_approx_term exp_approx_absx) (double exp_approx_i))) (set! exp_approx_sum (+' exp_approx_sum exp_approx_term)) (set! exp_approx_i (+' exp_approx_i 1)))) (if (< exp_approx_x 0.0) (/ 1.0 exp_approx_sum) exp_approx_sum)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn exponential_linear_unit [exponential_linear_unit_vector exponential_linear_unit_alpha]
  (binding [exponential_linear_unit_i nil exponential_linear_unit_neg nil exponential_linear_unit_result nil exponential_linear_unit_v nil] (try (do (set! exponential_linear_unit_result []) (set! exponential_linear_unit_i 0) (while (< exponential_linear_unit_i (count exponential_linear_unit_vector)) (do (set! exponential_linear_unit_v (nth exponential_linear_unit_vector exponential_linear_unit_i)) (if (> exponential_linear_unit_v 0.0) (set! exponential_linear_unit_result (conj exponential_linear_unit_result exponential_linear_unit_v)) (do (set! exponential_linear_unit_neg (*' exponential_linear_unit_alpha (- (exp_approx exponential_linear_unit_v) 1.0))) (set! exponential_linear_unit_result (conj exponential_linear_unit_result exponential_linear_unit_neg)))) (set! exponential_linear_unit_i (+' exponential_linear_unit_i 1)))) (throw (ex-info "return" {:v exponential_linear_unit_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (mochi_str (exponential_linear_unit [2.3 0.6 (- 2.0) (- 3.8)] 0.3)))
      (println (mochi_str (exponential_linear_unit [(- 9.2) (- 0.3) 0.45 (- 4.56)] 0.067)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
