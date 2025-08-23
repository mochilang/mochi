(ns main (:refer-clojure :exclude [int_sqrt sum_of_divisors solution]))

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

(declare int_sqrt sum_of_divisors solution)

(declare _read_file)

(def ^:dynamic int_sqrt_r nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_s nil)

(def ^:dynamic solution_total nil)

(def ^:dynamic sum_of_divisors_i nil)

(def ^:dynamic sum_of_divisors_root nil)

(def ^:dynamic sum_of_divisors_total nil)

(defn int_sqrt [int_sqrt_n]
  (binding [int_sqrt_r nil] (try (do (set! int_sqrt_r 0) (while (<= (* (+ int_sqrt_r 1) (+ int_sqrt_r 1)) int_sqrt_n) (set! int_sqrt_r (+ int_sqrt_r 1))) (throw (ex-info "return" {:v int_sqrt_r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sum_of_divisors [sum_of_divisors_n]
  (binding [sum_of_divisors_i nil sum_of_divisors_root nil sum_of_divisors_total nil] (try (do (set! sum_of_divisors_total 0) (set! sum_of_divisors_root (int_sqrt sum_of_divisors_n)) (set! sum_of_divisors_i 1) (while (<= sum_of_divisors_i sum_of_divisors_root) (do (when (= (mod sum_of_divisors_n sum_of_divisors_i) 0) (if (= (* sum_of_divisors_i sum_of_divisors_i) sum_of_divisors_n) (set! sum_of_divisors_total (+ sum_of_divisors_total sum_of_divisors_i)) (set! sum_of_divisors_total (+ (+ sum_of_divisors_total sum_of_divisors_i) (quot sum_of_divisors_n sum_of_divisors_i))))) (set! sum_of_divisors_i (+ sum_of_divisors_i 1)))) (throw (ex-info "return" {:v (- sum_of_divisors_total sum_of_divisors_n)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_limit]
  (binding [solution_i nil solution_s nil solution_total nil] (try (do (set! solution_total 0) (set! solution_i 1) (while (< solution_i solution_limit) (do (set! solution_s (sum_of_divisors solution_i)) (when (and (not= solution_s solution_i) (= (sum_of_divisors solution_s) solution_i)) (set! solution_total (+ solution_total solution_i))) (set! solution_i (+ solution_i 1)))) (throw (ex-info "return" {:v solution_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (solution 10000))
      (println (solution 5000))
      (println (solution 1000))
      (println (solution 100))
      (println (solution 50))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
