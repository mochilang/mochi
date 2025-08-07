(ns main (:refer-clojure :exclude [index_of_rightmost_set_bit]))

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

(declare index_of_rightmost_set_bit)

(def ^:dynamic index_of_rightmost_set_bit_index nil)

(def ^:dynamic index_of_rightmost_set_bit_n nil)

(defn index_of_rightmost_set_bit [index_of_rightmost_set_bit_number]
  (binding [index_of_rightmost_set_bit_index nil index_of_rightmost_set_bit_n nil] (try (do (when (< index_of_rightmost_set_bit_number 0) (throw (Exception. "Input must be a non-negative integer"))) (when (= index_of_rightmost_set_bit_number 0) (throw (ex-info "return" {:v (- 1)}))) (set! index_of_rightmost_set_bit_n index_of_rightmost_set_bit_number) (set! index_of_rightmost_set_bit_index 0) (while (= (mod index_of_rightmost_set_bit_n 2) 0) (do (set! index_of_rightmost_set_bit_n (quot index_of_rightmost_set_bit_n 2)) (set! index_of_rightmost_set_bit_index (+ index_of_rightmost_set_bit_index 1)))) (throw (ex-info "return" {:v index_of_rightmost_set_bit_index}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (index_of_rightmost_set_bit 0)))
      (println (str (index_of_rightmost_set_bit 5)))
      (println (str (index_of_rightmost_set_bit 36)))
      (println (str (index_of_rightmost_set_bit 8)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
