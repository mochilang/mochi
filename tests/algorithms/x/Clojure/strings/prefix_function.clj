(ns main (:refer-clojure :exclude [prefix_function longest_prefix list_eq_int test_prefix_function test_longest_prefix main]))

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

(declare prefix_function longest_prefix list_eq_int test_prefix_function test_longest_prefix main)

(def ^:dynamic list_eq_int_i nil)

(def ^:dynamic longest_prefix_i nil)

(def ^:dynamic longest_prefix_max_val nil)

(def ^:dynamic longest_prefix_pi nil)

(def ^:dynamic main_r1 nil)

(def ^:dynamic main_r2 nil)

(def ^:dynamic prefix_function_i nil)

(def ^:dynamic prefix_function_j nil)

(def ^:dynamic prefix_function_pi nil)

(def ^:dynamic test_prefix_function_expected1 nil)

(def ^:dynamic test_prefix_function_expected2 nil)

(def ^:dynamic test_prefix_function_r1 nil)

(def ^:dynamic test_prefix_function_r2 nil)

(def ^:dynamic test_prefix_function_s1 nil)

(def ^:dynamic test_prefix_function_s2 nil)

(defn prefix_function [prefix_function_s]
  (binding [prefix_function_i nil prefix_function_j nil prefix_function_pi nil] (try (do (set! prefix_function_pi []) (set! prefix_function_i 0) (while (< prefix_function_i (count prefix_function_s)) (do (set! prefix_function_pi (conj prefix_function_pi 0)) (set! prefix_function_i (+ prefix_function_i 1)))) (set! prefix_function_i 1) (while (< prefix_function_i (count prefix_function_s)) (do (set! prefix_function_j (nth prefix_function_pi (- prefix_function_i 1))) (while (and (> prefix_function_j 0) (not= (subs prefix_function_s prefix_function_i (+ prefix_function_i 1)) (subs prefix_function_s prefix_function_j (+ prefix_function_j 1)))) (set! prefix_function_j (nth prefix_function_pi (- prefix_function_j 1)))) (when (= (subs prefix_function_s prefix_function_i (+ prefix_function_i 1)) (subs prefix_function_s prefix_function_j (+ prefix_function_j 1))) (set! prefix_function_j (+ prefix_function_j 1))) (set! prefix_function_pi (assoc prefix_function_pi prefix_function_i prefix_function_j)) (set! prefix_function_i (+ prefix_function_i 1)))) (throw (ex-info "return" {:v prefix_function_pi}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn longest_prefix [longest_prefix_s]
  (binding [longest_prefix_i nil longest_prefix_max_val nil longest_prefix_pi nil] (try (do (set! longest_prefix_pi (prefix_function longest_prefix_s)) (set! longest_prefix_max_val 0) (set! longest_prefix_i 0) (while (< longest_prefix_i (count longest_prefix_pi)) (do (when (> (nth longest_prefix_pi longest_prefix_i) longest_prefix_max_val) (set! longest_prefix_max_val (nth longest_prefix_pi longest_prefix_i))) (set! longest_prefix_i (+ longest_prefix_i 1)))) (throw (ex-info "return" {:v longest_prefix_max_val}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn list_eq_int [list_eq_int_a list_eq_int_b]
  (binding [list_eq_int_i nil] (try (do (when (not= (count list_eq_int_a) (count list_eq_int_b)) (throw (ex-info "return" {:v false}))) (set! list_eq_int_i 0) (while (< list_eq_int_i (count list_eq_int_a)) (do (when (not= (nth list_eq_int_a list_eq_int_i) (nth list_eq_int_b list_eq_int_i)) (throw (ex-info "return" {:v false}))) (set! list_eq_int_i (+ list_eq_int_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_prefix_function []
  (binding [test_prefix_function_expected1 nil test_prefix_function_expected2 nil test_prefix_function_r1 nil test_prefix_function_r2 nil test_prefix_function_s1 nil test_prefix_function_s2 nil] (do (set! test_prefix_function_s1 "aabcdaabc") (set! test_prefix_function_expected1 [0 1 0 0 0 1 2 3 4]) (set! test_prefix_function_r1 (prefix_function test_prefix_function_s1)) (when (not (list_eq_int test_prefix_function_r1 test_prefix_function_expected1)) (throw (Exception. "prefix_function aabcdaabc failed"))) (set! test_prefix_function_s2 "asdasdad") (set! test_prefix_function_expected2 [0 0 0 1 2 3 4 0]) (set! test_prefix_function_r2 (prefix_function test_prefix_function_s2)) (when (not (list_eq_int test_prefix_function_r2 test_prefix_function_expected2)) (throw (Exception. "prefix_function asdasdad failed"))))))

(defn test_longest_prefix []
  (do (when (not= (longest_prefix "aabcdaabc") 4) (throw (Exception. "longest_prefix example1 failed"))) (when (not= (longest_prefix "asdasdad") 4) (throw (Exception. "longest_prefix example2 failed"))) (when (not= (longest_prefix "abcab") 2) (throw (Exception. "longest_prefix example3 failed")))))

(defn main []
  (binding [main_r1 nil main_r2 nil] (do (test_prefix_function) (test_longest_prefix) (set! main_r1 (prefix_function "aabcdaabc")) (set! main_r2 (prefix_function "asdasdad")) (println (str main_r1)) (println (str main_r2)) (println (str (longest_prefix "aabcdaabc"))) (println (str (longest_prefix "abcab"))))))

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
