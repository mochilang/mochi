(ns main (:refer-clojure :exclude [panic catalan_numbers]))

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

(declare panic catalan_numbers)

(def ^:dynamic catalan_numbers_catalans nil)

(def ^:dynamic catalan_numbers_j nil)

(def ^:dynamic catalan_numbers_n nil)

(def ^:dynamic catalan_numbers_next_val nil)

(defn panic [panic_msg]
  (println panic_msg))

(defn catalan_numbers [catalan_numbers_upper_limit]
  (binding [catalan_numbers_catalans nil catalan_numbers_j nil catalan_numbers_n nil catalan_numbers_next_val nil] (try (do (when (< catalan_numbers_upper_limit 0) (do (panic "Limit for the Catalan sequence must be >= 0") (throw (ex-info "return" {:v []})))) (set! catalan_numbers_catalans [1]) (set! catalan_numbers_n 1) (while (<= catalan_numbers_n catalan_numbers_upper_limit) (do (set! catalan_numbers_next_val 0) (set! catalan_numbers_j 0) (while (< catalan_numbers_j catalan_numbers_n) (do (set! catalan_numbers_next_val (+ catalan_numbers_next_val (* (nth catalan_numbers_catalans catalan_numbers_j) (nth catalan_numbers_catalans (- (- catalan_numbers_n catalan_numbers_j) 1))))) (set! catalan_numbers_j (+ catalan_numbers_j 1)))) (set! catalan_numbers_catalans (conj catalan_numbers_catalans catalan_numbers_next_val)) (set! catalan_numbers_n (+ catalan_numbers_n 1)))) (throw (ex-info "return" {:v catalan_numbers_catalans}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (catalan_numbers 5)))
      (println (str (catalan_numbers 2)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
