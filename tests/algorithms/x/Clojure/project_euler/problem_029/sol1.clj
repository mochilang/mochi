(ns main (:refer-clojure :exclude [int_pow solution main]))

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

(declare int_pow solution main)

(declare _read_file)

(def ^:dynamic int_pow_i nil)

(def ^:dynamic int_pow_result nil)

(def ^:dynamic main_n nil)

(def ^:dynamic solution_a nil)

(def ^:dynamic solution_b nil)

(def ^:dynamic solution_limit nil)

(def ^:dynamic solution_p nil)

(def ^:dynamic solution_powers nil)

(defn int_pow [int_pow_base int_pow_exp]
  (binding [int_pow_i nil int_pow_result nil] (try (do (set! int_pow_result 1) (set! int_pow_i 0) (while (< int_pow_i int_pow_exp) (do (set! int_pow_result (* int_pow_result int_pow_base)) (set! int_pow_i (+ int_pow_i 1)))) (throw (ex-info "return" {:v int_pow_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_n]
  (binding [solution_a nil solution_b nil solution_limit nil solution_p nil solution_powers nil] (try (do (set! solution_powers []) (set! solution_limit (+ solution_n 1)) (doseq [solution_a (range 2 solution_limit)] (doseq [solution_b (range 2 solution_limit)] (do (set! solution_p (int_pow solution_a solution_b)) (when (not (in solution_p solution_powers)) (set! solution_powers (conj solution_powers solution_p)))))) (throw (ex-info "return" {:v (count solution_powers)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_n nil] (do (set! main_n (toi (read-line))) (println "Number of terms " (solution main_n)))))

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
