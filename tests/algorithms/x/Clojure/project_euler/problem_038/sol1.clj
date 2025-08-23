(ns main (:refer-clojure :exclude [is_9_pandigital solution]))

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

(declare is_9_pandigital solution)

(declare _read_file)

(def ^:dynamic count_v nil)

(def ^:dynamic is_9_pandigital_digit nil)

(def ^:dynamic is_9_pandigital_digits nil)

(def ^:dynamic is_9_pandigital_i nil)

(def ^:dynamic is_9_pandigital_x nil)

(def ^:dynamic solution_base_num nil)

(def ^:dynamic solution_candidate nil)

(defn is_9_pandigital [is_9_pandigital_n]
  (binding [count_v nil is_9_pandigital_digit nil is_9_pandigital_digits nil is_9_pandigital_i nil is_9_pandigital_x nil] (try (do (set! is_9_pandigital_digits []) (set! is_9_pandigital_i 0) (while (< is_9_pandigital_i 10) (do (set! is_9_pandigital_digits (conj is_9_pandigital_digits 0)) (set! is_9_pandigital_i (+ is_9_pandigital_i 1)))) (set! count_v 0) (set! is_9_pandigital_x is_9_pandigital_n) (while (> is_9_pandigital_x 0) (do (set! is_9_pandigital_digit (mod is_9_pandigital_x 10)) (when (= is_9_pandigital_digit 0) (throw (ex-info "return" {:v false}))) (when (= (nth is_9_pandigital_digits is_9_pandigital_digit) 1) (throw (ex-info "return" {:v false}))) (set! is_9_pandigital_digits (assoc is_9_pandigital_digits is_9_pandigital_digit 1)) (set! is_9_pandigital_x (quot is_9_pandigital_x 10)) (set! count_v (+ count_v 1)))) (throw (ex-info "return" {:v (and (and (and (and (and (and (and (and (and (= count_v 9) (= (nth is_9_pandigital_digits 1) 1)) (= (nth is_9_pandigital_digits 2) 1)) (= (nth is_9_pandigital_digits 3) 1)) (= (nth is_9_pandigital_digits 4) 1)) (= (nth is_9_pandigital_digits 5) 1)) (= (nth is_9_pandigital_digits 6) 1)) (= (nth is_9_pandigital_digits 7) 1)) (= (nth is_9_pandigital_digits 8) 1)) (= (nth is_9_pandigital_digits 9) 1))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution []
  (binding [solution_base_num nil solution_candidate nil] (try (do (set! solution_base_num 9999) (while (>= solution_base_num 5000) (do (set! solution_candidate (* 100002 solution_base_num)) (when (is_9_pandigital solution_candidate) (throw (ex-info "return" {:v solution_candidate}))) (set! solution_base_num (- solution_base_num 1)))) (set! solution_base_num 333) (while (>= solution_base_num 100) (do (set! solution_candidate (* 1002003 solution_base_num)) (when (is_9_pandigital solution_candidate) (throw (ex-info "return" {:v solution_candidate}))) (set! solution_base_num (- solution_base_num 1)))) (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "solution() = " (mochi_str (solution))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
