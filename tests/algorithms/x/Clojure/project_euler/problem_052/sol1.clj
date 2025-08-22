(ns main (:refer-clojure :exclude [digits_count equal_lists solution]))

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

(declare digits_count equal_lists solution)

(declare _read_file)

(def ^:dynamic digits_count_counts nil)

(def ^:dynamic digits_count_d nil)

(def ^:dynamic digits_count_i nil)

(def ^:dynamic digits_count_x nil)

(def ^:dynamic equal_lists_i nil)

(def ^:dynamic solution_c nil)

(def ^:dynamic solution_i nil)

(defn digits_count [digits_count_n]
  (binding [digits_count_counts nil digits_count_d nil digits_count_i nil digits_count_x nil] (try (do (set! digits_count_counts []) (set! digits_count_i 0) (while (< digits_count_i 10) (do (set! digits_count_counts (conj digits_count_counts 0)) (set! digits_count_i (+ digits_count_i 1)))) (set! digits_count_x digits_count_n) (when (= digits_count_x 0) (set! digits_count_counts (assoc digits_count_counts 0 (+ (nth digits_count_counts 0) 1)))) (while (> digits_count_x 0) (do (set! digits_count_d (mod digits_count_x 10)) (set! digits_count_counts (assoc digits_count_counts digits_count_d (+ (nth digits_count_counts digits_count_d) 1))) (set! digits_count_x (/ digits_count_x 10)))) (throw (ex-info "return" {:v digits_count_counts}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn equal_lists [equal_lists_a equal_lists_b]
  (binding [equal_lists_i nil] (try (do (set! equal_lists_i 0) (while (< equal_lists_i (count equal_lists_a)) (do (when (not= (nth equal_lists_a equal_lists_i) (nth equal_lists_b equal_lists_i)) (throw (ex-info "return" {:v false}))) (set! equal_lists_i (+ equal_lists_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution []
  (binding [solution_c nil solution_i nil] (try (do (set! solution_i 1) (while true (do (set! solution_c (digits_count solution_i)) (when (and (and (and (and (equal_lists solution_c (digits_count (* 2 solution_i))) (equal_lists solution_c (digits_count (* 3 solution_i)))) (equal_lists solution_c (digits_count (* 4 solution_i)))) (equal_lists solution_c (digits_count (* 5 solution_i)))) (equal_lists solution_c (digits_count (* 6 solution_i)))) (throw (ex-info "return" {:v solution_i}))) (set! solution_i (+ solution_i 1))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

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
