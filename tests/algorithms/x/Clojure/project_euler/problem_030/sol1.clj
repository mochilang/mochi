(ns main (:refer-clojure :exclude [digits_fifth_powers_sum solution]))

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

(declare digits_fifth_powers_sum solution)

(declare _read_file)

(def ^:dynamic digits_fifth_powers_sum_digit nil)

(def ^:dynamic digits_fifth_powers_sum_n nil)

(def ^:dynamic digits_fifth_powers_sum_total nil)

(def ^:dynamic solution_num nil)

(def ^:dynamic solution_total nil)

(def ^:dynamic main_DIGITS_FIFTH_POWER nil)

(defn digits_fifth_powers_sum [digits_fifth_powers_sum_number]
  (binding [digits_fifth_powers_sum_digit nil digits_fifth_powers_sum_n nil digits_fifth_powers_sum_total nil] (try (do (set! digits_fifth_powers_sum_total 0) (set! digits_fifth_powers_sum_n digits_fifth_powers_sum_number) (while (> digits_fifth_powers_sum_n 0) (do (set! digits_fifth_powers_sum_digit (mod digits_fifth_powers_sum_n 10)) (set! digits_fifth_powers_sum_total (+ digits_fifth_powers_sum_total (nth main_DIGITS_FIFTH_POWER digits_fifth_powers_sum_digit))) (set! digits_fifth_powers_sum_n (quot digits_fifth_powers_sum_n 10)))) (throw (ex-info "return" {:v digits_fifth_powers_sum_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution []
  (binding [solution_num nil solution_total nil] (try (do (set! solution_total 0) (set! solution_num 1000) (while (< solution_num 1000000) (do (when (= solution_num (digits_fifth_powers_sum solution_num)) (set! solution_total (+ solution_total solution_num))) (set! solution_num (+ solution_num 1)))) (throw (ex-info "return" {:v solution_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_DIGITS_FIFTH_POWER) (constantly [0 1 32 243 1024 3125 7776 16807 32768 59049]))
      (println (solution))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
