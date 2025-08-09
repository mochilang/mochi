(ns main (:refer-clojure :exclude [hexagonal_numbers test_hexagonal_numbers]))

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

(declare hexagonal_numbers test_hexagonal_numbers)

(def ^:dynamic hexagonal_numbers_n nil)

(def ^:dynamic hexagonal_numbers_res nil)

(def ^:dynamic test_hexagonal_numbers_expected10 nil)

(def ^:dynamic test_hexagonal_numbers_expected5 nil)

(def ^:dynamic test_hexagonal_numbers_result10 nil)

(def ^:dynamic test_hexagonal_numbers_result5 nil)

(defn hexagonal_numbers [hexagonal_numbers_length]
  (binding [hexagonal_numbers_n nil hexagonal_numbers_res nil] (try (do (when (<= hexagonal_numbers_length 0) (throw (Exception. "Length must be a positive integer."))) (set! hexagonal_numbers_res []) (set! hexagonal_numbers_n 0) (while (< hexagonal_numbers_n hexagonal_numbers_length) (do (set! hexagonal_numbers_res (conj hexagonal_numbers_res (* hexagonal_numbers_n (- (* 2 hexagonal_numbers_n) 1)))) (set! hexagonal_numbers_n (+ hexagonal_numbers_n 1)))) (throw (ex-info "return" {:v hexagonal_numbers_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_hexagonal_numbers []
  (binding [test_hexagonal_numbers_expected10 nil test_hexagonal_numbers_expected5 nil test_hexagonal_numbers_result10 nil test_hexagonal_numbers_result5 nil] (do (set! test_hexagonal_numbers_expected5 [0 1 6 15 28]) (set! test_hexagonal_numbers_result5 (hexagonal_numbers 5)) (when (not= test_hexagonal_numbers_result5 test_hexagonal_numbers_expected5) (throw (Exception. "hexagonal_numbers(5) failed"))) (set! test_hexagonal_numbers_expected10 [0 1 6 15 28 45 66 91 120 153]) (set! test_hexagonal_numbers_result10 (hexagonal_numbers 10)) (when (not= test_hexagonal_numbers_result10 test_hexagonal_numbers_expected10) (throw (Exception. "hexagonal_numbers(10) failed"))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (test_hexagonal_numbers)
      (println (str (hexagonal_numbers 5)))
      (println (str (hexagonal_numbers 10)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
