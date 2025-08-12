(ns main (:refer-clojure :exclude [is_prime solution main]))

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
  (Integer/parseInt (str s)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare is_prime solution main)

(def ^:dynamic is_prime_i nil)

(def ^:dynamic main_result nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_max_number nil)

(def ^:dynamic solution_num nil)

(defn is_prime [is_prime_number]
  (binding [is_prime_i nil] (try (do (when (and (> is_prime_number 1) (< is_prime_number 4)) (throw (ex-info "return" {:v true}))) (when (or (or (< is_prime_number 2) (= (mod is_prime_number 2) 0)) (= (mod is_prime_number 3) 0)) (throw (ex-info "return" {:v false}))) (set! is_prime_i 5) (while (<= (* is_prime_i is_prime_i) is_prime_number) (do (when (or (= (mod is_prime_number is_prime_i) 0) (= (mod is_prime_number (+ is_prime_i 2)) 0)) (throw (ex-info "return" {:v false}))) (set! is_prime_i (+ is_prime_i 6)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_n]
  (binding [solution_i nil solution_max_number nil solution_num nil] (try (do (set! solution_num solution_n) (when (<= solution_num 0) (do (println "Parameter n must be greater than or equal to one.") (throw (ex-info "return" {:v 0})))) (when (is_prime solution_num) (throw (ex-info "return" {:v solution_num}))) (while (= (mod solution_num 2) 0) (do (set! solution_num (/ solution_num 2)) (when (is_prime solution_num) (throw (ex-info "return" {:v solution_num}))))) (set! solution_max_number 1) (set! solution_i 3) (loop [while_flag_1 true] (when (and while_flag_1 (<= (* solution_i solution_i) solution_num)) (do (when (= (mod solution_num solution_i) 0) (if (is_prime (/ solution_num solution_i)) (do (set! solution_max_number (/ solution_num solution_i)) (recur false)) (when (is_prime solution_i) (set! solution_max_number solution_i)))) (set! solution_i (+ solution_i 2)) (cond :else (recur while_flag_1))))) (throw (ex-info "return" {:v solution_max_number}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_result nil] (do (set! main_result (solution 600851475143)) (println (str "solution() = " (str main_result))))))

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
