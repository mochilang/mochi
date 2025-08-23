(ns main (:refer-clojure :exclude [int_sqrt solution]))

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

(declare int_sqrt solution)

(declare _read_file)

(def ^:dynamic int_sqrt_x nil)

(def ^:dynamic solution_a nil)

(def ^:dynamic solution_abundants nil)

(def ^:dynamic solution_b nil)

(def ^:dynamic solution_has_pair nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_is_abundant nil)

(def ^:dynamic solution_j nil)

(def ^:dynamic solution_k nil)

(def ^:dynamic solution_n nil)

(def ^:dynamic solution_res nil)

(def ^:dynamic solution_sqrt_limit nil)

(def ^:dynamic solution_sum_divs nil)

(defn int_sqrt [int_sqrt_n]
  (binding [int_sqrt_x nil] (try (do (set! int_sqrt_x 1) (while (<= (* (+ int_sqrt_x 1) (+ int_sqrt_x 1)) int_sqrt_n) (set! int_sqrt_x (+ int_sqrt_x 1))) (throw (ex-info "return" {:v int_sqrt_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_limit]
  (binding [solution_a nil solution_abundants nil solution_b nil solution_has_pair nil solution_i nil solution_is_abundant nil solution_j nil solution_k nil solution_n nil solution_res nil solution_sqrt_limit nil solution_sum_divs nil] (try (do (set! solution_sum_divs []) (set! solution_i 0) (while (<= solution_i solution_limit) (do (set! solution_sum_divs (conj solution_sum_divs 1)) (set! solution_i (+ solution_i 1)))) (set! solution_sqrt_limit (int_sqrt solution_limit)) (set! solution_i 2) (while (<= solution_i solution_sqrt_limit) (do (set! solution_sum_divs (assoc solution_sum_divs (* solution_i solution_i) (+ (nth solution_sum_divs (* solution_i solution_i)) solution_i))) (set! solution_k (+ solution_i 1)) (while (<= solution_k (quot solution_limit solution_i)) (do (set! solution_sum_divs (assoc solution_sum_divs (* solution_k solution_i) (+ (+ (nth solution_sum_divs (* solution_k solution_i)) solution_k) solution_i))) (set! solution_k (+ solution_k 1)))) (set! solution_i (+ solution_i 1)))) (set! solution_is_abundant []) (set! solution_i 0) (while (<= solution_i solution_limit) (do (set! solution_is_abundant (conj solution_is_abundant false)) (set! solution_i (+ solution_i 1)))) (set! solution_abundants []) (set! solution_res 0) (set! solution_n 1) (loop [while_flag_1 true] (when (and while_flag_1 (<= solution_n solution_limit)) (do (when (> (nth solution_sum_divs solution_n) solution_n) (do (set! solution_abundants (conj solution_abundants solution_n)) (set! solution_is_abundant (assoc solution_is_abundant solution_n true)))) (set! solution_has_pair false) (set! solution_j 0) (loop [while_flag_2 true] (when (and while_flag_2 (< solution_j (count solution_abundants))) (do (set! solution_a (nth solution_abundants solution_j)) (if (> solution_a solution_n) (recur false) (do (set! solution_b (- solution_n solution_a)) (if (and (<= solution_b solution_limit) (nth solution_is_abundant solution_b)) (do (set! solution_has_pair true) (recur false)) (set! solution_j (+ solution_j 1))))) (cond :else (recur while_flag_2))))) (when (not solution_has_pair) (set! solution_res (+ solution_res solution_n))) (set! solution_n (+ solution_n 1)) (cond :else (recur while_flag_1))))) (throw (ex-info "return" {:v solution_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (mochi_str (solution 28123)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
