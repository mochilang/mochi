(ns main (:refer-clojure :exclude [collatz_length solution]))

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

(declare collatz_length solution)

(declare _read_file)

(def ^:dynamic collatz_length_i nil)

(def ^:dynamic collatz_length_length nil)

(def ^:dynamic collatz_length_num nil)

(def ^:dynamic collatz_length_sequence nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_length nil)

(def ^:dynamic solution_max_len nil)

(def ^:dynamic solution_max_start nil)

(def ^:dynamic main_collatz_cache nil)

(defn collatz_length [collatz_length_n]
  (binding [collatz_length_i nil collatz_length_length nil collatz_length_num nil collatz_length_sequence nil] (try (do (set! collatz_length_num collatz_length_n) (set! collatz_length_sequence []) (while (not (in collatz_length_num main_collatz_cache)) (do (set! collatz_length_sequence (conj collatz_length_sequence collatz_length_num)) (if (= (mod collatz_length_num 2) 0) (set! collatz_length_num (long (quot collatz_length_num 2))) (set! collatz_length_num (+ (* 3 collatz_length_num) 1))))) (set! collatz_length_length (get main_collatz_cache collatz_length_num)) (set! collatz_length_i (- (count collatz_length_sequence) 1)) (while (>= collatz_length_i 0) (do (set! collatz_length_length (+ collatz_length_length 1)) (alter-var-root (var main_collatz_cache) (fn [_] (assoc main_collatz_cache (nth collatz_length_sequence collatz_length_i) collatz_length_length))) (set! collatz_length_i (- collatz_length_i 1)))) (throw (ex-info "return" {:v collatz_length_length}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_limit]
  (binding [solution_i nil solution_length nil solution_max_len nil solution_max_start nil] (try (do (set! solution_max_len 0) (set! solution_max_start 1) (set! solution_i 1) (while (< solution_i solution_limit) (do (set! solution_length (collatz_length solution_i)) (when (> solution_length solution_max_len) (do (set! solution_max_len solution_length) (set! solution_max_start solution_i))) (set! solution_i (+ solution_i 1)))) (throw (ex-info "return" {:v solution_max_start}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_input_str nil)

(def ^:dynamic main_limit nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_collatz_cache) (constantly {1 1}))
      (alter-var-root (var main_input_str) (constantly (read-line)))
      (alter-var-root (var main_limit) (constantly (toi main_input_str)))
      (println (solution main_limit))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
