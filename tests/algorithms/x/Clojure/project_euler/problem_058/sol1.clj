(ns main (:refer-clojure :exclude [is_prime solution test_solution main]))

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

(declare is_prime solution test_solution main)

(declare _read_file)

(def ^:dynamic is_prime_i nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_j nil)

(def ^:dynamic solution_limit nil)

(def ^:dynamic solution_primes nil)

(def ^:dynamic solution_step nil)

(defn is_prime [is_prime_number]
  (binding [is_prime_i nil] (try (do (when (and (< 1 is_prime_number) (< is_prime_number 4)) (throw (ex-info "return" {:v true}))) (when (or (or (< is_prime_number 2) (= (mod is_prime_number 2) 0)) (= (mod is_prime_number 3) 0)) (throw (ex-info "return" {:v false}))) (set! is_prime_i 5) (while (<= (* is_prime_i is_prime_i) is_prime_number) (do (when (or (= (mod is_prime_number is_prime_i) 0) (= (mod is_prime_number (+ is_prime_i 2)) 0)) (throw (ex-info "return" {:v false}))) (set! is_prime_i (+ is_prime_i 6)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_ratio]
  (binding [solution_i nil solution_j nil solution_limit nil solution_primes nil solution_step nil] (try (do (set! solution_j 3) (set! solution_primes 3) (while (>= (/ (double solution_primes) (- (* 2 solution_j) 1)) solution_ratio) (do (set! solution_i (+ (+ (* solution_j solution_j) solution_j) 1)) (set! solution_limit (* (+ solution_j 2) (+ solution_j 2))) (set! solution_step (+ solution_j 1)) (while (< solution_i solution_limit) (do (when (is_prime solution_i) (set! solution_primes (+ solution_primes 1))) (set! solution_i (+ solution_i solution_step)))) (set! solution_j (+ solution_j 2)))) (throw (ex-info "return" {:v solution_j}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_solution []
  (do (when (not= (solution 0.5) 11) (throw (Exception. "solution 0.5 failed"))) (when (not= (solution 0.2) 309) (throw (Exception. "solution 0.2 failed"))) (when (not= (solution 0.111) 11317) (throw (Exception. "solution 0.111 failed")))))

(defn main []
  (do (test_solution) (println (mochi_str (solution 0.1)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (main)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
