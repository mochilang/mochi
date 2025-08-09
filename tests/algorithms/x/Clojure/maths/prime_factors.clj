(ns main (:refer-clojure :exclude [prime_factors list_eq test_prime_factors main]))

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

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare prime_factors list_eq test_prime_factors main)

(def ^:dynamic list_eq_i nil)

(def ^:dynamic prime_factors_factors nil)

(def ^:dynamic prime_factors_i nil)

(def ^:dynamic prime_factors_num nil)

(defn prime_factors [prime_factors_n]
  (binding [prime_factors_factors nil prime_factors_i nil prime_factors_num nil] (try (do (when (< prime_factors_n 2) (throw (ex-info "return" {:v []}))) (set! prime_factors_num prime_factors_n) (set! prime_factors_i 2) (set! prime_factors_factors []) (while (<= (* prime_factors_i prime_factors_i) prime_factors_num) (if (= (mod prime_factors_num prime_factors_i) 0) (do (set! prime_factors_factors (conj prime_factors_factors prime_factors_i)) (set! prime_factors_num (quot prime_factors_num prime_factors_i))) (set! prime_factors_i (+ prime_factors_i 1)))) (when (> prime_factors_num 1) (set! prime_factors_factors (conj prime_factors_factors prime_factors_num))) (throw (ex-info "return" {:v prime_factors_factors}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn list_eq [list_eq_a list_eq_b]
  (binding [list_eq_i nil] (try (do (when (not= (count list_eq_a) (count list_eq_b)) (throw (ex-info "return" {:v false}))) (set! list_eq_i 0) (while (< list_eq_i (count list_eq_a)) (do (when (not= (nth list_eq_a list_eq_i) (nth list_eq_b list_eq_i)) (throw (ex-info "return" {:v false}))) (set! list_eq_i (+ list_eq_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_prime_factors []
  (do (when (not (list_eq (prime_factors 0) [])) (throw (Exception. "prime_factors(0) failed"))) (when (not (list_eq (prime_factors 100) [2 2 5 5])) (throw (Exception. "prime_factors(100) failed"))) (when (not (list_eq (prime_factors 2560) [2 2 2 2 2 2 2 2 2 5])) (throw (Exception. "prime_factors(2560) failed"))) (when (not (list_eq (prime_factors 97) [97])) (throw (Exception. "prime_factors(97) failed")))))

(defn main []
  (do (test_prime_factors) (println (str (prime_factors 100))) (println (str (prime_factors 2560))) (println (str (prime_factors 97)))))

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
