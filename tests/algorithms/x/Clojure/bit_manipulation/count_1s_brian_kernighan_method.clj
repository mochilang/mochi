(ns main (:refer-clojure :exclude [lowest_set_bit get_1s_count]))

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

(declare lowest_set_bit get_1s_count)

(def ^:dynamic count_v nil)

(def ^:dynamic get_1s_count_n nil)

(def ^:dynamic lowest_set_bit_lb nil)

(defn lowest_set_bit [lowest_set_bit_n]
  (binding [lowest_set_bit_lb nil] (try (do (set! lowest_set_bit_lb 1) (while (= (mod lowest_set_bit_n (* lowest_set_bit_lb 2)) 0) (set! lowest_set_bit_lb (* lowest_set_bit_lb 2))) (throw (ex-info "return" {:v lowest_set_bit_lb}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_1s_count [get_1s_count_number]
  (binding [count_v nil get_1s_count_n nil] (try (do (when (< get_1s_count_number 0) (do (println "ValueError: Input must be a non-negative integer") (throw (ex-info "return" {:v 0})))) (set! get_1s_count_n get_1s_count_number) (set! count_v 0) (while (> get_1s_count_n 0) (do (set! get_1s_count_n (- get_1s_count_n (lowest_set_bit get_1s_count_n))) (set! count_v (+ count_v 1)))) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (get_1s_count 25)))
      (println (str (get_1s_count 37)))
      (println (str (get_1s_count 21)))
      (println (str (get_1s_count 58)))
      (println (str (get_1s_count 0)))
      (println (str (get_1s_count 256)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
