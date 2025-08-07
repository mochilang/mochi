(ns main (:refer-clojure :exclude [largest_pow_of_two_le_num]))

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

(declare largest_pow_of_two_le_num)

(def ^:dynamic largest_pow_of_two_le_num_res nil)

(defn largest_pow_of_two_le_num [largest_pow_of_two_le_num_n]
  (binding [largest_pow_of_two_le_num_res nil] (try (do (when (<= largest_pow_of_two_le_num_n 0) (throw (ex-info "return" {:v 0}))) (set! largest_pow_of_two_le_num_res 1) (while (<= (* largest_pow_of_two_le_num_res 2) largest_pow_of_two_le_num_n) (set! largest_pow_of_two_le_num_res (* largest_pow_of_two_le_num_res 2))) (throw (ex-info "return" {:v largest_pow_of_two_le_num_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (largest_pow_of_two_le_num 0)))
      (println (str (largest_pow_of_two_le_num 1)))
      (println (str (largest_pow_of_two_le_num (- 1))))
      (println (str (largest_pow_of_two_le_num 3)))
      (println (str (largest_pow_of_two_le_num 15)))
      (println (str (largest_pow_of_two_le_num 99)))
      (println (str (largest_pow_of_two_le_num 178)))
      (println (str (largest_pow_of_two_le_num 999999)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
