(ns main (:refer-clojure :exclude [solution]))

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

(declare solution)

(declare _read_file)

(def ^:dynamic solution_counter nil)

(def ^:dynamic solution_counters nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_largest_number nil)

(def ^:dynamic solution_number nil)

(def ^:dynamic solution_pre_counter nil)

(def ^:dynamic solution_start nil)

(defn solution [solution_n]
  (binding [solution_counter nil solution_counters nil solution_i nil solution_largest_number nil solution_number nil solution_pre_counter nil solution_start nil] (try (do (set! solution_counters []) (set! solution_i 0) (while (<= solution_i solution_n) (do (set! solution_counters (conj solution_counters 0)) (set! solution_i (+ solution_i 1)))) (set! solution_counters (assoc solution_counters 1 1)) (set! solution_largest_number 1) (set! solution_pre_counter 1) (set! solution_start 2) (loop [while_flag_1 true] (when (and while_flag_1 (< solution_start solution_n)) (do (set! solution_number solution_start) (set! solution_counter 0) (loop [while_flag_2 true] (when (and while_flag_2 true) (do (if (and (< solution_number (count solution_counters)) (not= (nth solution_counters solution_number) 0)) (do (set! solution_counter (+ solution_counter (nth solution_counters solution_number))) (recur false)) (do (if (= (mod solution_number 2) 0) (set! solution_number (quot solution_number 2)) (set! solution_number (+ (* 3 solution_number) 1))) (set! solution_counter (+ solution_counter 1)))) (cond :else (recur while_flag_2))))) (when (and (< solution_start (count solution_counters)) (= (nth solution_counters solution_start) 0)) (set! solution_counters (assoc solution_counters solution_start solution_counter))) (when (> solution_counter solution_pre_counter) (do (set! solution_largest_number solution_start) (set! solution_pre_counter solution_counter))) (set! solution_start (+ solution_start 1)) (cond :else (recur while_flag_1))))) (throw (ex-info "return" {:v solution_largest_number}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_input_str nil)

(def ^:dynamic main_n nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_input_str) (constantly (read-line)))
      (alter-var-root (var main_n) (constantly (toi main_input_str)))
      (println (mochi_str (solution main_n)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
