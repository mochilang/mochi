(ns main (:refer-clojure :exclude [find_missing_number]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare find_missing_number)

(def ^:dynamic count_v nil)

(def ^:dynamic find_missing_number_actual_sum nil)

(def ^:dynamic find_missing_number_expected_sum nil)

(def ^:dynamic find_missing_number_high nil)

(def ^:dynamic find_missing_number_i nil)

(def ^:dynamic find_missing_number_low nil)

(def ^:dynamic find_missing_number_n nil)

(defn find_missing_number [find_missing_number_nums]
  (binding [count_v nil find_missing_number_actual_sum nil find_missing_number_expected_sum nil find_missing_number_high nil find_missing_number_i nil find_missing_number_low nil find_missing_number_n nil] (try (do (set! find_missing_number_low (long (apply min find_missing_number_nums))) (set! find_missing_number_high (long (apply max find_missing_number_nums))) (set! count_v (+ (- find_missing_number_high find_missing_number_low) 1)) (set! find_missing_number_expected_sum (quot (* (+ find_missing_number_low find_missing_number_high) count_v) 2)) (set! find_missing_number_actual_sum 0) (set! find_missing_number_i 0) (set! find_missing_number_n (count find_missing_number_nums)) (while (< find_missing_number_i find_missing_number_n) (do (set! find_missing_number_actual_sum (+ find_missing_number_actual_sum (nth find_missing_number_nums find_missing_number_i))) (set! find_missing_number_i (+ find_missing_number_i 1)))) (throw (ex-info "return" {:v (- find_missing_number_expected_sum find_missing_number_actual_sum)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (find_missing_number [0 1 3 4]))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
