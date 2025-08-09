(ns main (:refer-clojure :exclude [abs_int greatest_common_divisor gcd_by_iterative]))

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

(declare abs_int greatest_common_divisor gcd_by_iterative)

(def ^:dynamic gcd_by_iterative_a nil)

(def ^:dynamic gcd_by_iterative_b nil)

(def ^:dynamic gcd_by_iterative_temp nil)

(def ^:dynamic greatest_common_divisor_x nil)

(def ^:dynamic greatest_common_divisor_y nil)

(defn abs_int [abs_int_n]
  (try (if (< abs_int_n 0) (- abs_int_n) abs_int_n) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn greatest_common_divisor [greatest_common_divisor_a greatest_common_divisor_b]
  (binding [greatest_common_divisor_x nil greatest_common_divisor_y nil] (try (do (set! greatest_common_divisor_x (abs_int greatest_common_divisor_a)) (set! greatest_common_divisor_y (abs_int greatest_common_divisor_b)) (if (= greatest_common_divisor_x 0) greatest_common_divisor_y (greatest_common_divisor (mod greatest_common_divisor_y greatest_common_divisor_x) greatest_common_divisor_x))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn gcd_by_iterative [gcd_by_iterative_x gcd_by_iterative_y]
  (binding [gcd_by_iterative_a nil gcd_by_iterative_b nil gcd_by_iterative_temp nil] (try (do (set! gcd_by_iterative_a (abs_int gcd_by_iterative_x)) (set! gcd_by_iterative_b (abs_int gcd_by_iterative_y)) (while (not= gcd_by_iterative_b 0) (do (set! gcd_by_iterative_temp gcd_by_iterative_b) (set! gcd_by_iterative_b (mod gcd_by_iterative_a gcd_by_iterative_b)) (set! gcd_by_iterative_a gcd_by_iterative_temp))) (throw (ex-info "return" {:v gcd_by_iterative_a}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (greatest_common_divisor 24 40)))
      (println (str (greatest_common_divisor 1 1)))
      (println (str (greatest_common_divisor 1 800)))
      (println (str (greatest_common_divisor 11 37)))
      (println (str (greatest_common_divisor 3 5)))
      (println (str (greatest_common_divisor 16 4)))
      (println (str (greatest_common_divisor (- 3) 9)))
      (println (str (greatest_common_divisor 9 (- 3))))
      (println (str (greatest_common_divisor 3 (- 9))))
      (println (str (greatest_common_divisor (- 3) (- 9))))
      (println (str (gcd_by_iterative 24 40)))
      (println (str (= (greatest_common_divisor 24 40) (gcd_by_iterative 24 40))))
      (println (str (gcd_by_iterative (- 3) (- 9))))
      (println (str (gcd_by_iterative 3 (- 9))))
      (println (str (gcd_by_iterative 1 (- 800))))
      (println (str (gcd_by_iterative 11 37)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
