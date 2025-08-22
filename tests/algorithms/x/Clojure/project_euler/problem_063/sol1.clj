(ns main (:refer-clojure :exclude [pow num_digits solution]))

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

(declare pow num_digits solution)

(declare _read_file)

(def ^:dynamic count_v nil)

(def ^:dynamic num_digits_x nil)

(def ^:dynamic pow_i nil)

(def ^:dynamic pow_result nil)

(def ^:dynamic solution_base nil)

(def ^:dynamic solution_digits nil)

(def ^:dynamic solution_power nil)

(def ^:dynamic solution_total nil)

(defn pow [pow_base pow_exponent]
  (binding [pow_i nil pow_result nil] (try (do (set! pow_result 1) (set! pow_i 0) (while (< pow_i pow_exponent) (do (set! pow_result (* pow_result pow_base)) (set! pow_i (+ pow_i 1)))) (throw (ex-info "return" {:v pow_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn num_digits [num_digits_n]
  (binding [count_v nil num_digits_x nil] (try (do (when (= num_digits_n 0) (throw (ex-info "return" {:v 1}))) (set! count_v 0) (set! num_digits_x num_digits_n) (while (> num_digits_x 0) (do (set! num_digits_x (/ num_digits_x 10)) (set! count_v (+ count_v 1)))) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_max_base solution_max_power]
  (binding [solution_base nil solution_digits nil solution_power nil solution_total nil] (try (do (set! solution_total 0) (set! solution_base 1) (while (< solution_base solution_max_base) (do (set! solution_power 1) (while (< solution_power solution_max_power) (do (set! solution_digits (num_digits (pow solution_base solution_power))) (when (= solution_digits solution_power) (set! solution_total (+ solution_total 1))) (set! solution_power (+ solution_power 1)))) (set! solution_base (+ solution_base 1)))) (throw (ex-info "return" {:v solution_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "solution(10, 22) = " (mochi_str (solution 10 22))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
