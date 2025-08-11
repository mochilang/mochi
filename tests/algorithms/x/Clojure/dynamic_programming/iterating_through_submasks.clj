(ns main (:refer-clojure :exclude [bitwise_and list_of_submasks]))

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

(declare bitwise_and list_of_submasks)

(def ^:dynamic bitwise_and_abit nil)

(def ^:dynamic bitwise_and_bbit nil)

(def ^:dynamic bitwise_and_bit nil)

(def ^:dynamic bitwise_and_result nil)

(def ^:dynamic bitwise_and_x nil)

(def ^:dynamic bitwise_and_y nil)

(def ^:dynamic list_of_submasks_all_submasks nil)

(def ^:dynamic list_of_submasks_submask nil)

(defn bitwise_and [bitwise_and_a bitwise_and_b]
  (binding [bitwise_and_abit nil bitwise_and_bbit nil bitwise_and_bit nil bitwise_and_result nil bitwise_and_x nil bitwise_and_y nil] (try (do (set! bitwise_and_result 0) (set! bitwise_and_bit 1) (set! bitwise_and_x bitwise_and_a) (set! bitwise_and_y bitwise_and_b) (while (or (> bitwise_and_x 0) (> bitwise_and_y 0)) (do (set! bitwise_and_abit (mod bitwise_and_x 2)) (set! bitwise_and_bbit (mod bitwise_and_y 2)) (when (and (= bitwise_and_abit 1) (= bitwise_and_bbit 1)) (set! bitwise_and_result (+ bitwise_and_result bitwise_and_bit))) (set! bitwise_and_x (/ bitwise_and_x 2)) (set! bitwise_and_y (/ bitwise_and_y 2)) (set! bitwise_and_bit (* bitwise_and_bit 2)))) (throw (ex-info "return" {:v bitwise_and_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn list_of_submasks [list_of_submasks_mask]
  (binding [list_of_submasks_all_submasks nil list_of_submasks_submask nil] (try (do (when (<= list_of_submasks_mask 0) (throw (Exception. (str "mask needs to be positive integer, your input " (str list_of_submasks_mask))))) (set! list_of_submasks_all_submasks []) (set! list_of_submasks_submask list_of_submasks_mask) (while (not= list_of_submasks_submask 0) (do (set! list_of_submasks_all_submasks (conj list_of_submasks_all_submasks list_of_submasks_submask)) (set! list_of_submasks_submask (bitwise_and (- list_of_submasks_submask 1) list_of_submasks_mask)))) (throw (ex-info "return" {:v list_of_submasks_all_submasks}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (list_of_submasks 15)))
      (println (str (list_of_submasks 13)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
