(ns main (:refer-clojure :exclude [count_divisors solution]))

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

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare count_divisors solution)

(def ^:dynamic count_divisors_i nil)

(def ^:dynamic count_divisors_m nil)

(def ^:dynamic count_divisors_multiplicity nil)

(def ^:dynamic count_divisors_n_divisors nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_t_num nil)

(defn count_divisors [count_divisors_n]
  (binding [count_divisors_i nil count_divisors_m nil count_divisors_multiplicity nil count_divisors_n_divisors nil] (try (do (set! count_divisors_m count_divisors_n) (set! count_divisors_n_divisors 1) (set! count_divisors_i 2) (while (<= (* count_divisors_i count_divisors_i) count_divisors_m) (do (set! count_divisors_multiplicity 0) (while (= (mod count_divisors_m count_divisors_i) 0) (do (set! count_divisors_m (/ count_divisors_m count_divisors_i)) (set! count_divisors_multiplicity (+ count_divisors_multiplicity 1)))) (set! count_divisors_n_divisors (* count_divisors_n_divisors (+ count_divisors_multiplicity 1))) (set! count_divisors_i (+ count_divisors_i 1)))) (when (> count_divisors_m 1) (set! count_divisors_n_divisors (* count_divisors_n_divisors 2))) (throw (ex-info "return" {:v count_divisors_n_divisors}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution []
  (binding [solution_i nil solution_t_num nil] (try (do (set! solution_t_num 1) (set! solution_i 1) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (set! solution_i (+ solution_i 1)) (set! solution_t_num (+ solution_t_num solution_i)) (cond (> (count_divisors solution_t_num) 500) (recur false) :else (recur while_flag_1))))) (throw (ex-info "return" {:v solution_t_num}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
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
