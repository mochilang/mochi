(ns main (:refer-clojure :exclude [sum_digits solution]))

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

(declare sum_digits solution)

(declare _read_file)

(def ^:dynamic solution_cur_numerator nil)

(def ^:dynamic solution_e_cont nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_pre_numerator nil)

(def ^:dynamic solution_temp nil)

(def ^:dynamic sum_digits_digit_sum nil)

(def ^:dynamic sum_digits_n nil)

(defn sum_digits [sum_digits_num]
  (binding [sum_digits_digit_sum nil sum_digits_n nil] (try (do (set! sum_digits_n sum_digits_num) (set! sum_digits_digit_sum 0) (while (> sum_digits_n 0) (do (set! sum_digits_digit_sum (+ sum_digits_digit_sum (mod sum_digits_n 10))) (set! sum_digits_n (/ sum_digits_n 10)))) (throw (ex-info "return" {:v sum_digits_digit_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_max_n]
  (binding [solution_cur_numerator nil solution_e_cont nil solution_i nil solution_pre_numerator nil solution_temp nil] (try (do (set! solution_pre_numerator 1) (set! solution_cur_numerator 2) (set! solution_i 2) (while (<= solution_i solution_max_n) (do (set! solution_temp solution_pre_numerator) (set! solution_e_cont 1) (when (= (mod solution_i 3) 0) (set! solution_e_cont (/ (* 2 solution_i) 3))) (set! solution_pre_numerator solution_cur_numerator) (set! solution_cur_numerator (+ (* solution_e_cont solution_pre_numerator) solution_temp)) (set! solution_i (+ solution_i 1)))) (throw (ex-info "return" {:v (sum_digits solution_cur_numerator)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (solution 9))
      (println (solution 10))
      (println (solution 50))
      (println (solution 100))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
