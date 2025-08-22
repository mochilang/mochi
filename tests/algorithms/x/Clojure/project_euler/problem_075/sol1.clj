(ns main (:refer-clojure :exclude [gcd solution]))

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

(declare gcd solution)

(declare _read_file)

(def ^:dynamic count_v nil)

(def ^:dynamic gcd_t nil)

(def ^:dynamic gcd_x nil)

(def ^:dynamic gcd_y nil)

(def ^:dynamic solution_frequencies nil)

(def ^:dynamic solution_m nil)

(def ^:dynamic solution_n nil)

(def ^:dynamic solution_p nil)

(def ^:dynamic solution_perimeter nil)

(def ^:dynamic solution_primitive_perimeter nil)

(defn gcd [gcd_a gcd_b]
  (binding [gcd_t nil gcd_x nil gcd_y nil] (try (do (set! gcd_x gcd_a) (set! gcd_y gcd_b) (while (not= gcd_y 0) (do (set! gcd_t (mod gcd_x gcd_y)) (set! gcd_x gcd_y) (set! gcd_y gcd_t))) (throw (ex-info "return" {:v gcd_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_limit]
  (binding [count_v nil solution_frequencies nil solution_m nil solution_n nil solution_p nil solution_perimeter nil solution_primitive_perimeter nil] (try (do (set! solution_frequencies {}) (set! solution_m 2) (loop [while_flag_1 true] (when (and while_flag_1 (<= (* (* 2 solution_m) (+ solution_m 1)) solution_limit)) (do (set! solution_n (+ (mod solution_m 2) 1)) (loop [while_flag_2 true] (when (and while_flag_2 (< solution_n solution_m)) (cond (> (gcd solution_m solution_n) 1) (do (set! solution_n (+ solution_n 2)) (recur true)) :else (do (set! solution_primitive_perimeter (* (* 2 solution_m) (+ solution_m solution_n))) (set! solution_perimeter solution_primitive_perimeter) (while (<= solution_perimeter solution_limit) (do (when (not (in solution_perimeter solution_frequencies)) (set! solution_frequencies (assoc solution_frequencies solution_perimeter 0))) (set! solution_frequencies (assoc solution_frequencies solution_perimeter (+ (get solution_frequencies solution_perimeter) 1))) (set! solution_perimeter (+ solution_perimeter solution_primitive_perimeter)))) (set! solution_n (+ solution_n 2)) (recur while_flag_2))))) (set! solution_m (+ solution_m 1)) (cond :else (do))))) (set! count_v 0) (doseq [solution_p (keys solution_frequencies)] (when (= (get solution_frequencies solution_p) 1) (set! count_v (+ count_v 1)))) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_result nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_result) (constantly (solution 1500000)))
      (println (str "solution() = " (mochi_str main_result)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
