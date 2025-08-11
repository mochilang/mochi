(ns main (:refer-clojure :exclude [z_function go_next find_pattern list_eq_int test_z_function test_find_pattern main]))

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

(declare z_function go_next find_pattern list_eq_int test_z_function test_find_pattern main)

(def ^:dynamic find_pattern_answer nil)

(def ^:dynamic find_pattern_i nil)

(def ^:dynamic find_pattern_z_res nil)

(def ^:dynamic list_eq_int_i nil)

(def ^:dynamic main_r1 nil)

(def ^:dynamic main_r2 nil)

(def ^:dynamic main_r3 nil)

(def ^:dynamic test_z_function_expected1 nil)

(def ^:dynamic test_z_function_expected2 nil)

(def ^:dynamic test_z_function_expected3 nil)

(def ^:dynamic test_z_function_r1 nil)

(def ^:dynamic test_z_function_r2 nil)

(def ^:dynamic test_z_function_r3 nil)

(def ^:dynamic test_z_function_s1 nil)

(def ^:dynamic test_z_function_s2 nil)

(def ^:dynamic test_z_function_s3 nil)

(def ^:dynamic z_function_i nil)

(def ^:dynamic z_function_l nil)

(def ^:dynamic z_function_min_edge nil)

(def ^:dynamic z_function_r nil)

(def ^:dynamic z_function_z nil)

(def ^:dynamic z_function_zi nil)

(defn z_function [z_function_s]
  (binding [z_function_i nil z_function_l nil z_function_min_edge nil z_function_r nil z_function_z nil z_function_zi nil] (try (do (set! z_function_z []) (set! z_function_i 0) (while (< z_function_i (count z_function_s)) (do (set! z_function_z (conj z_function_z 0)) (set! z_function_i (+ z_function_i 1)))) (set! z_function_l 0) (set! z_function_r 0) (set! z_function_i 1) (while (< z_function_i (count z_function_s)) (do (when (<= z_function_i z_function_r) (do (set! z_function_min_edge (+ (- z_function_r z_function_i) 1)) (set! z_function_zi (nth z_function_z (- z_function_i z_function_l))) (when (< z_function_zi z_function_min_edge) (set! z_function_min_edge z_function_zi)) (set! z_function_z (assoc z_function_z z_function_i z_function_min_edge)))) (while (go_next z_function_i z_function_z z_function_s) (set! z_function_z (assoc z_function_z z_function_i (+ (nth z_function_z z_function_i) 1)))) (when (> (- (+ z_function_i (nth z_function_z z_function_i)) 1) z_function_r) (do (set! z_function_l z_function_i) (set! z_function_r (- (+ z_function_i (nth z_function_z z_function_i)) 1)))) (set! z_function_i (+ z_function_i 1)))) (throw (ex-info "return" {:v z_function_z}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn go_next [go_next_i go_next_z go_next_s]
  (try (throw (ex-info "return" {:v (and (< (+ go_next_i (nth go_next_z go_next_i)) (count go_next_s)) (= (subs go_next_s (nth go_next_z go_next_i) (+ (nth go_next_z go_next_i) 1)) (subs go_next_s (+ go_next_i (nth go_next_z go_next_i)) (+ (+ go_next_i (nth go_next_z go_next_i)) 1))))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn find_pattern [find_pattern_pattern find_pattern_input_str]
  (binding [find_pattern_answer nil find_pattern_i nil find_pattern_z_res nil] (try (do (set! find_pattern_answer 0) (set! find_pattern_z_res (z_function (str find_pattern_pattern find_pattern_input_str))) (set! find_pattern_i 0) (while (< find_pattern_i (count find_pattern_z_res)) (do (when (>= (nth find_pattern_z_res find_pattern_i) (count find_pattern_pattern)) (set! find_pattern_answer (+ find_pattern_answer 1))) (set! find_pattern_i (+ find_pattern_i 1)))) (throw (ex-info "return" {:v find_pattern_answer}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn list_eq_int [list_eq_int_a list_eq_int_b]
  (binding [list_eq_int_i nil] (try (do (when (not= (count list_eq_int_a) (count list_eq_int_b)) (throw (ex-info "return" {:v false}))) (set! list_eq_int_i 0) (while (< list_eq_int_i (count list_eq_int_a)) (do (when (not= (nth list_eq_int_a list_eq_int_i) (nth list_eq_int_b list_eq_int_i)) (throw (ex-info "return" {:v false}))) (set! list_eq_int_i (+ list_eq_int_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_z_function []
  (binding [test_z_function_expected1 nil test_z_function_expected2 nil test_z_function_expected3 nil test_z_function_r1 nil test_z_function_r2 nil test_z_function_r3 nil test_z_function_s1 nil test_z_function_s2 nil test_z_function_s3 nil] (do (set! test_z_function_s1 "abracadabra") (set! test_z_function_expected1 [0 0 0 1 0 1 0 4 0 0 1]) (set! test_z_function_r1 (z_function test_z_function_s1)) (when (not (list_eq_int test_z_function_r1 test_z_function_expected1)) (throw (Exception. "z_function abracadabra failed"))) (set! test_z_function_s2 "aaaa") (set! test_z_function_expected2 [0 3 2 1]) (set! test_z_function_r2 (z_function test_z_function_s2)) (when (not (list_eq_int test_z_function_r2 test_z_function_expected2)) (throw (Exception. "z_function aaaa failed"))) (set! test_z_function_s3 "zxxzxxz") (set! test_z_function_expected3 [0 0 0 4 0 0 1]) (set! test_z_function_r3 (z_function test_z_function_s3)) (when (not (list_eq_int test_z_function_r3 test_z_function_expected3)) (throw (Exception. "z_function zxxzxxz failed"))))))

(defn test_find_pattern []
  (do (when (not= (find_pattern "abr" "abracadabra") 2) (throw (Exception. "find_pattern abr failed"))) (when (not= (find_pattern "a" "aaaa") 4) (throw (Exception. "find_pattern aaaa failed"))) (when (not= (find_pattern "xz" "zxxzxxz") 2) (throw (Exception. "find_pattern xz failed")))))

(defn main []
  (binding [main_r1 nil main_r2 nil main_r3 nil] (do (test_z_function) (test_find_pattern) (set! main_r1 (z_function "abracadabra")) (set! main_r2 (z_function "aaaa")) (set! main_r3 (z_function "zxxzxxz")) (println (str main_r1)) (println (str main_r2)) (println (str main_r3)) (println (str (find_pattern "abr" "abracadabra"))) (println (str (find_pattern "a" "aaaa"))) (println (str (find_pattern "xz" "zxxzxxz"))))))

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
