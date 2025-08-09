(ns main (:refer-clojure :exclude [gcd get_greatest_common_divisor]))

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

(declare gcd get_greatest_common_divisor)

(def ^:dynamic gcd_r nil)

(def ^:dynamic gcd_x nil)

(def ^:dynamic gcd_y nil)

(def ^:dynamic get_greatest_common_divisor_g nil)

(def ^:dynamic get_greatest_common_divisor_i nil)

(def ^:dynamic get_greatest_common_divisor_n nil)

(defn gcd [gcd_a gcd_b]
  (binding [gcd_r nil gcd_x nil gcd_y nil] (try (do (set! gcd_x gcd_a) (set! gcd_y gcd_b) (while (not= gcd_y 0) (do (set! gcd_r (mod gcd_x gcd_y)) (set! gcd_x gcd_y) (set! gcd_y gcd_r))) (if (< gcd_x 0) (- gcd_x) gcd_x)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_greatest_common_divisor [get_greatest_common_divisor_nums]
  (binding [get_greatest_common_divisor_g nil get_greatest_common_divisor_i nil get_greatest_common_divisor_n nil] (try (do (when (= (count get_greatest_common_divisor_nums) 0) (throw (Exception. "at least one number is required"))) (set! get_greatest_common_divisor_g (nth get_greatest_common_divisor_nums 0)) (when (<= get_greatest_common_divisor_g 0) (throw (Exception. "numbers must be integer and greater than zero"))) (set! get_greatest_common_divisor_i 1) (while (< get_greatest_common_divisor_i (count get_greatest_common_divisor_nums)) (do (set! get_greatest_common_divisor_n (nth get_greatest_common_divisor_nums get_greatest_common_divisor_i)) (when (<= get_greatest_common_divisor_n 0) (throw (Exception. "numbers must be integer and greater than zero"))) (set! get_greatest_common_divisor_g (gcd get_greatest_common_divisor_g get_greatest_common_divisor_n)) (set! get_greatest_common_divisor_i (+ get_greatest_common_divisor_i 1)))) (throw (ex-info "return" {:v get_greatest_common_divisor_g}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (get_greatest_common_divisor [18 45])))
      (println (str (get_greatest_common_divisor [23 37])))
      (println (str (get_greatest_common_divisor [2520 8350])))
      (println (str (get_greatest_common_divisor [1 2 3 4 5 6 7 8 9 10])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
