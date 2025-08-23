(ns main (:refer-clojure :exclude [sum_of_digit_factorial solution]))

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

(declare sum_of_digit_factorial solution)

(declare _read_file)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_limit nil)

(def ^:dynamic solution_total nil)

(def ^:dynamic sum_of_digit_factorial_digit nil)

(def ^:dynamic sum_of_digit_factorial_num nil)

(def ^:dynamic sum_of_digit_factorial_total nil)

(def ^:dynamic main_DIGIT_FACTORIALS nil)

(defn sum_of_digit_factorial [sum_of_digit_factorial_n]
  (binding [sum_of_digit_factorial_digit nil sum_of_digit_factorial_num nil sum_of_digit_factorial_total nil] (try (do (when (= sum_of_digit_factorial_n 0) (throw (ex-info "return" {:v (nth main_DIGIT_FACTORIALS 0)}))) (set! sum_of_digit_factorial_total 0) (set! sum_of_digit_factorial_num sum_of_digit_factorial_n) (while (> sum_of_digit_factorial_num 0) (do (set! sum_of_digit_factorial_digit (mod sum_of_digit_factorial_num 10)) (set! sum_of_digit_factorial_total (+ sum_of_digit_factorial_total (nth main_DIGIT_FACTORIALS sum_of_digit_factorial_digit))) (set! sum_of_digit_factorial_num (quot sum_of_digit_factorial_num 10)))) (throw (ex-info "return" {:v sum_of_digit_factorial_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution []
  (binding [solution_i nil solution_limit nil solution_total nil] (try (do (set! solution_limit (+ (* 7 (nth main_DIGIT_FACTORIALS 9)) 1)) (set! solution_total 0) (set! solution_i 3) (while (< solution_i solution_limit) (do (when (= (sum_of_digit_factorial solution_i) solution_i) (set! solution_total (+ solution_total solution_i))) (set! solution_i (+ solution_i 1)))) (throw (ex-info "return" {:v solution_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_DIGIT_FACTORIALS) (constantly [1 1 2 6 24 120 720 5040 40320 362880]))
      (println (mochi_str (solution)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
