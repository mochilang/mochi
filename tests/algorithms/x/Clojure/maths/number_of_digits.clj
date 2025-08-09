(ns main (:refer-clojure :exclude [abs_int num_digits num_digits_fast num_digits_faster test_num_digits main]))

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

(declare abs_int num_digits num_digits_fast num_digits_faster test_num_digits main)

(def ^:dynamic num_digits_digits nil)

(def ^:dynamic num_digits_fast_digits nil)

(def ^:dynamic num_digits_fast_power nil)

(def ^:dynamic num_digits_fast_x nil)

(def ^:dynamic num_digits_faster_s nil)

(def ^:dynamic num_digits_x nil)

(defn abs_int [abs_int_n]
  (try (if (< abs_int_n 0) (- abs_int_n) abs_int_n) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn num_digits [num_digits_n]
  (binding [num_digits_digits nil num_digits_x nil] (try (do (set! num_digits_x (abs_int num_digits_n)) (set! num_digits_digits 1) (while (>= num_digits_x 10) (do (set! num_digits_x (quot num_digits_x 10)) (set! num_digits_digits (+ num_digits_digits 1)))) (throw (ex-info "return" {:v num_digits_digits}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn num_digits_fast [num_digits_fast_n]
  (binding [num_digits_fast_digits nil num_digits_fast_power nil num_digits_fast_x nil] (try (do (set! num_digits_fast_x (abs_int num_digits_fast_n)) (set! num_digits_fast_digits 1) (set! num_digits_fast_power 10) (while (>= num_digits_fast_x num_digits_fast_power) (do (set! num_digits_fast_power (* num_digits_fast_power 10)) (set! num_digits_fast_digits (+ num_digits_fast_digits 1)))) (throw (ex-info "return" {:v num_digits_fast_digits}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn num_digits_faster [num_digits_faster_n]
  (binding [num_digits_faster_s nil] (try (do (set! num_digits_faster_s (str (abs_int num_digits_faster_n))) (throw (ex-info "return" {:v (count num_digits_faster_s)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_num_digits []
  (do (when (not= (num_digits 12345) 5) (throw (Exception. "num_digits 12345 failed"))) (when (not= (num_digits 123) 3) (throw (Exception. "num_digits 123 failed"))) (when (not= (num_digits 0) 1) (throw (Exception. "num_digits 0 failed"))) (when (not= (num_digits (- 1)) 1) (throw (Exception. "num_digits -1 failed"))) (when (not= (num_digits (- 123456)) 6) (throw (Exception. "num_digits -123456 failed"))) (when (not= (num_digits_fast 12345) 5) (throw (Exception. "num_digits_fast 12345 failed"))) (when (not= (num_digits_fast 123) 3) (throw (Exception. "num_digits_fast 123 failed"))) (when (not= (num_digits_fast 0) 1) (throw (Exception. "num_digits_fast 0 failed"))) (when (not= (num_digits_fast (- 1)) 1) (throw (Exception. "num_digits_fast -1 failed"))) (when (not= (num_digits_fast (- 123456)) 6) (throw (Exception. "num_digits_fast -123456 failed"))) (when (not= (num_digits_faster 12345) 5) (throw (Exception. "num_digits_faster 12345 failed"))) (when (not= (num_digits_faster 123) 3) (throw (Exception. "num_digits_faster 123 failed"))) (when (not= (num_digits_faster 0) 1) (throw (Exception. "num_digits_faster 0 failed"))) (when (not= (num_digits_faster (- 1)) 1) (throw (Exception. "num_digits_faster -1 failed"))) (when (not= (num_digits_faster (- 123456)) 6) (throw (Exception. "num_digits_faster -123456 failed")))))

(defn main []
  (do (test_num_digits) (println (str (num_digits 12345))) (println (str (num_digits_fast 12345))) (println (str (num_digits_faster 12345)))))

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
