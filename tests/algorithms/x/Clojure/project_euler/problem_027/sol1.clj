(ns main (:refer-clojure :exclude [is_prime solution]))

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

(declare is_prime solution)

(declare _read_file)

(def ^:dynamic count_v nil)

(def ^:dynamic is_prime_i nil)

(def ^:dynamic solution_a nil)

(def ^:dynamic solution_b nil)

(def ^:dynamic solution_longest_a nil)

(def ^:dynamic solution_longest_b nil)

(def ^:dynamic solution_longest_len nil)

(def ^:dynamic solution_n nil)

(defn is_prime [is_prime_number]
  (binding [is_prime_i nil] (try (do (if (and (< 1 is_prime_number) (< is_prime_number 4)) (throw (ex-info "return" {:v true})) (when (or (or (< is_prime_number 2) (= (mod is_prime_number 2) 0)) (= (mod is_prime_number 3) 0)) (throw (ex-info "return" {:v false})))) (set! is_prime_i 5) (while (<= (* is_prime_i is_prime_i) is_prime_number) (do (when (or (= (mod is_prime_number is_prime_i) 0) (= (mod is_prime_number (+ is_prime_i 2)) 0)) (throw (ex-info "return" {:v false}))) (set! is_prime_i (+ is_prime_i 6)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_a_limit solution_b_limit]
  (binding [count_v nil solution_a nil solution_b nil solution_longest_a nil solution_longest_b nil solution_longest_len nil solution_n nil] (try (do (set! solution_longest_len 0) (set! solution_longest_a 0) (set! solution_longest_b 0) (set! solution_a (+ (* (- 1) solution_a_limit) 1)) (while (< solution_a solution_a_limit) (do (set! solution_b 2) (while (< solution_b solution_b_limit) (do (when (is_prime solution_b) (do (set! count_v 0) (set! solution_n 0) (while (is_prime (+ (+ (* solution_n solution_n) (* solution_a solution_n)) solution_b)) (do (set! count_v (+ count_v 1)) (set! solution_n (+ solution_n 1)))) (when (> count_v solution_longest_len) (do (set! solution_longest_len count_v) (set! solution_longest_a solution_a) (set! solution_longest_b solution_b))))) (set! solution_b (+ solution_b 1)))) (set! solution_a (+ solution_a 1)))) (throw (ex-info "return" {:v (* solution_longest_a solution_longest_b)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (mochi_str (solution 1000 1000)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
