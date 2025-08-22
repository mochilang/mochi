(ns main (:refer-clojure :exclude [get_totients has_same_digits solution]))

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

(declare get_totients has_same_digits solution)

(declare _read_file)

(def ^:dynamic get_totients_i nil)

(def ^:dynamic get_totients_totients nil)

(def ^:dynamic get_totients_x nil)

(def ^:dynamic has_same_digits_count1 nil)

(def ^:dynamic has_same_digits_count2 nil)

(def ^:dynamic has_same_digits_d1 nil)

(def ^:dynamic has_same_digits_d2 nil)

(def ^:dynamic has_same_digits_i nil)

(def ^:dynamic has_same_digits_n1 nil)

(def ^:dynamic has_same_digits_n2 nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_min_denominator nil)

(def ^:dynamic solution_min_numerator nil)

(def ^:dynamic solution_t nil)

(def ^:dynamic solution_totients nil)

(defn get_totients [get_totients_max_one]
  (binding [get_totients_i nil get_totients_totients nil get_totients_x nil] (try (do (set! get_totients_totients []) (set! get_totients_i 0) (while (< get_totients_i get_totients_max_one) (do (set! get_totients_totients (conj get_totients_totients get_totients_i)) (set! get_totients_i (+ get_totients_i 1)))) (set! get_totients_i 2) (while (< get_totients_i get_totients_max_one) (do (when (= (nth get_totients_totients get_totients_i) get_totients_i) (do (set! get_totients_x get_totients_i) (while (< get_totients_x get_totients_max_one) (do (set! get_totients_totients (assoc get_totients_totients get_totients_x (- (nth get_totients_totients get_totients_x) (/ (nth get_totients_totients get_totients_x) get_totients_i)))) (set! get_totients_x (+ get_totients_x get_totients_i)))))) (set! get_totients_i (+ get_totients_i 1)))) (throw (ex-info "return" {:v get_totients_totients}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn has_same_digits [has_same_digits_num1 has_same_digits_num2]
  (binding [has_same_digits_count1 nil has_same_digits_count2 nil has_same_digits_d1 nil has_same_digits_d2 nil has_same_digits_i nil has_same_digits_n1 nil has_same_digits_n2 nil] (try (do (set! has_same_digits_count1 []) (set! has_same_digits_count2 []) (set! has_same_digits_i 0) (while (< has_same_digits_i 10) (do (set! has_same_digits_count1 (conj has_same_digits_count1 0)) (set! has_same_digits_count2 (conj has_same_digits_count2 0)) (set! has_same_digits_i (+ has_same_digits_i 1)))) (set! has_same_digits_n1 has_same_digits_num1) (set! has_same_digits_n2 has_same_digits_num2) (when (= has_same_digits_n1 0) (set! has_same_digits_count1 (assoc has_same_digits_count1 0 (+ (nth has_same_digits_count1 0) 1)))) (when (= has_same_digits_n2 0) (set! has_same_digits_count2 (assoc has_same_digits_count2 0 (+ (nth has_same_digits_count2 0) 1)))) (while (> has_same_digits_n1 0) (do (set! has_same_digits_d1 (mod has_same_digits_n1 10)) (set! has_same_digits_count1 (assoc has_same_digits_count1 has_same_digits_d1 (+ (nth has_same_digits_count1 has_same_digits_d1) 1))) (set! has_same_digits_n1 (/ has_same_digits_n1 10)))) (while (> has_same_digits_n2 0) (do (set! has_same_digits_d2 (mod has_same_digits_n2 10)) (set! has_same_digits_count2 (assoc has_same_digits_count2 has_same_digits_d2 (+ (nth has_same_digits_count2 has_same_digits_d2) 1))) (set! has_same_digits_n2 (/ has_same_digits_n2 10)))) (set! has_same_digits_i 0) (while (< has_same_digits_i 10) (do (when (not= (nth has_same_digits_count1 has_same_digits_i) (nth has_same_digits_count2 has_same_digits_i)) (throw (ex-info "return" {:v false}))) (set! has_same_digits_i (+ has_same_digits_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_max_n]
  (binding [solution_i nil solution_min_denominator nil solution_min_numerator nil solution_t nil solution_totients nil] (try (do (set! solution_min_numerator 1) (set! solution_min_denominator 0) (set! solution_totients (get_totients (+ solution_max_n 1))) (set! solution_i 2) (while (<= solution_i solution_max_n) (do (set! solution_t (nth solution_totients solution_i)) (when (and (< (* solution_i solution_min_denominator) (* solution_min_numerator solution_t)) (has_same_digits solution_i solution_t)) (do (set! solution_min_numerator solution_i) (set! solution_min_denominator solution_t))) (set! solution_i (+ solution_i 1)))) (throw (ex-info "return" {:v solution_min_numerator}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (mochi_str (solution 10000)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
