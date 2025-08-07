(ns main (:refer-clojure :exclude [highest_set_bit_position]))

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

(declare highest_set_bit_position)

(def ^:dynamic highest_set_bit_position_n nil)

(def ^:dynamic highest_set_bit_position_position nil)

(defn highest_set_bit_position [highest_set_bit_position_number]
  (binding [highest_set_bit_position_n nil highest_set_bit_position_position nil] (try (do (when (< highest_set_bit_position_number 0) (throw (Exception. "number must be non-negative"))) (set! highest_set_bit_position_position 0) (set! highest_set_bit_position_n highest_set_bit_position_number) (while (> highest_set_bit_position_n 0) (do (set! highest_set_bit_position_position (+ highest_set_bit_position_position 1)) (set! highest_set_bit_position_n (quot highest_set_bit_position_n 2)))) (throw (ex-info "return" {:v highest_set_bit_position_position}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (highest_set_bit_position 25)))
      (println (str (highest_set_bit_position 37)))
      (println (str (highest_set_bit_position 1)))
      (println (str (highest_set_bit_position 4)))
      (println (str (highest_set_bit_position 0)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
