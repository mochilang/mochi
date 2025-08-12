(ns main (:refer-clojure :exclude [isqrt is_prime solution main]))

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

(declare isqrt is_prime solution main)

(def ^:dynamic count_v nil)

(def ^:dynamic is_prime_i nil)

(def ^:dynamic is_prime_limit nil)

(def ^:dynamic isqrt_r nil)

(def ^:dynamic solution_number nil)

(defn isqrt [isqrt_n]
  (binding [isqrt_r nil] (try (do (set! isqrt_r 0) (while (<= (* (+ isqrt_r 1) (+ isqrt_r 1)) isqrt_n) (set! isqrt_r (+ isqrt_r 1))) (throw (ex-info "return" {:v isqrt_r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_prime [is_prime_number]
  (binding [is_prime_i nil is_prime_limit nil] (try (do (if (and (< 1 is_prime_number) (< is_prime_number 4)) (throw (ex-info "return" {:v true})) (when (or (or (< is_prime_number 2) (= (mod is_prime_number 2) 0)) (= (mod is_prime_number 3) 0)) (throw (ex-info "return" {:v false})))) (set! is_prime_limit (isqrt is_prime_number)) (set! is_prime_i 5) (while (<= is_prime_i is_prime_limit) (do (when (or (= (mod is_prime_number is_prime_i) 0) (= (mod is_prime_number (+ is_prime_i 2)) 0)) (throw (ex-info "return" {:v false}))) (set! is_prime_i (+ is_prime_i 6)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_nth]
  (binding [count_v nil solution_number nil] (try (do (set! count_v 0) (set! solution_number 1) (while (and (not= count_v solution_nth) (< solution_number 3)) (do (set! solution_number (+ solution_number 1)) (when (is_prime solution_number) (set! count_v (+ count_v 1))))) (while (not= count_v solution_nth) (do (set! solution_number (+ solution_number 2)) (when (is_prime solution_number) (set! count_v (+ count_v 1))))) (throw (ex-info "return" {:v solution_number}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (println (str "solution() = " (str (solution 10001)))))

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
