(ns main (:refer-clojure :exclude [get_digits solution]))

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

(declare get_digits solution)

(declare _read_file)

(def ^:dynamic get_digits_c nil)

(def ^:dynamic get_digits_counts nil)

(def ^:dynamic get_digits_cube nil)

(def ^:dynamic get_digits_d nil)

(def ^:dynamic get_digits_i nil)

(def ^:dynamic get_digits_j nil)

(def ^:dynamic get_digits_result nil)

(def ^:dynamic get_digits_s nil)

(def ^:dynamic solution_arr nil)

(def ^:dynamic solution_base nil)

(def ^:dynamic solution_digits nil)

(def ^:dynamic solution_freqs nil)

(def ^:dynamic solution_num nil)

(defn get_digits [get_digits_num]
  (binding [get_digits_c nil get_digits_counts nil get_digits_cube nil get_digits_d nil get_digits_i nil get_digits_j nil get_digits_result nil get_digits_s nil] (try (do (set! get_digits_cube (* (* get_digits_num get_digits_num) get_digits_num)) (set! get_digits_s (mochi_str get_digits_cube)) (set! get_digits_counts []) (set! get_digits_j 0) (while (< get_digits_j 10) (do (set! get_digits_counts (conj get_digits_counts 0)) (set! get_digits_j (+ get_digits_j 1)))) (set! get_digits_i 0) (while (< get_digits_i (count get_digits_s)) (do (set! get_digits_d (toi (nth get_digits_s get_digits_i))) (set! get_digits_counts (assoc get_digits_counts get_digits_d (+ (nth get_digits_counts get_digits_d) 1))) (set! get_digits_i (+ get_digits_i 1)))) (set! get_digits_result "") (set! get_digits_d 0) (while (< get_digits_d 10) (do (set! get_digits_c (nth get_digits_counts get_digits_d)) (while (> get_digits_c 0) (do (set! get_digits_result (str get_digits_result (mochi_str get_digits_d))) (set! get_digits_c (- get_digits_c 1)))) (set! get_digits_d (+ get_digits_d 1)))) (throw (ex-info "return" {:v get_digits_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_max_base]
  (binding [solution_arr nil solution_base nil solution_digits nil solution_freqs nil solution_num nil] (try (do (set! solution_freqs {}) (set! solution_num 0) (while true (do (set! solution_digits (get_digits solution_num)) (set! solution_arr []) (when (in solution_digits solution_freqs) (set! solution_arr (get solution_freqs solution_digits))) (set! solution_arr (conj solution_arr solution_num)) (set! solution_freqs (assoc solution_freqs solution_digits solution_arr)) (when (= (count solution_arr) solution_max_base) (do (set! solution_base (get solution_arr 0)) (throw (ex-info "return" {:v (* (* solution_base solution_base) solution_base)})))) (set! solution_num (+ solution_num 1))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "solution() = " (mochi_str (solution 5))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
