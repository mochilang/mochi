(ns main (:refer-clojure :exclude [pow_int digital_sum solution test_solution main]))

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

(declare pow_int digital_sum solution test_solution main)

(declare _read_file)

(def ^:dynamic digital_sum_i nil)

(def ^:dynamic digital_sum_s nil)

(def ^:dynamic digital_sum_sum nil)

(def ^:dynamic pow_int_i nil)

(def ^:dynamic pow_int_result nil)

(def ^:dynamic solution_base nil)

(def ^:dynamic solution_ds nil)

(def ^:dynamic solution_max_sum nil)

(def ^:dynamic solution_power nil)

(def ^:dynamic solution_value nil)

(defn pow_int [pow_int_base pow_int_exp]
  (binding [pow_int_i nil pow_int_result nil] (try (do (set! pow_int_result 1) (set! pow_int_i 0) (while (< pow_int_i pow_int_exp) (do (set! pow_int_result (* pow_int_result pow_int_base)) (set! pow_int_i (+ pow_int_i 1)))) (throw (ex-info "return" {:v pow_int_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn digital_sum [digital_sum_n]
  (binding [digital_sum_i nil digital_sum_s nil digital_sum_sum nil] (try (do (set! digital_sum_s (mochi_str digital_sum_n)) (set! digital_sum_sum 0) (set! digital_sum_i 0) (while (< digital_sum_i (count digital_sum_s)) (do (set! digital_sum_sum (+ digital_sum_sum (long (nth digital_sum_s digital_sum_i)))) (set! digital_sum_i (+ digital_sum_i 1)))) (throw (ex-info "return" {:v digital_sum_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_a solution_b]
  (binding [solution_base nil solution_ds nil solution_max_sum nil solution_power nil solution_value nil] (try (do (set! solution_max_sum 0) (set! solution_base 0) (while (< solution_base solution_a) (do (set! solution_power 0) (while (< solution_power solution_b) (do (set! solution_value (pow_int solution_base solution_power)) (set! solution_ds (digital_sum solution_value)) (when (> solution_ds solution_max_sum) (set! solution_max_sum solution_ds)) (set! solution_power (+ solution_power 1)))) (set! solution_base (+ solution_base 1)))) (throw (ex-info "return" {:v solution_max_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_solution []
  (do (when (not= (solution 10 10) 45) (throw (Exception. "solution 10 10 failed"))) (when (not= (solution 100 100) 972) (throw (Exception. "solution 100 100 failed"))) (when (not= (solution 100 200) 1872) (throw (Exception. "solution 100 200 failed")))))

(defn main []
  (do (test_solution) (println (mochi_str (solution 100 100)))))

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
