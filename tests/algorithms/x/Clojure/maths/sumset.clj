(ns main (:refer-clojure :exclude [contains sumset main]))

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

(declare contains sumset main)

(def ^:dynamic contains_i nil)

(def ^:dynamic main_set_a nil)

(def ^:dynamic main_set_b nil)

(def ^:dynamic main_set_c nil)

(def ^:dynamic sumset_i nil)

(def ^:dynamic sumset_j nil)

(def ^:dynamic sumset_result nil)

(def ^:dynamic sumset_s nil)

(defn contains [contains_xs contains_value]
  (binding [contains_i nil] (try (do (set! contains_i 0) (while (< contains_i (count contains_xs)) (do (when (= (nth contains_xs contains_i) contains_value) (throw (ex-info "return" {:v true}))) (set! contains_i (+ contains_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sumset [sumset_set_a sumset_set_b]
  (binding [sumset_i nil sumset_j nil sumset_result nil sumset_s nil] (try (do (set! sumset_result []) (set! sumset_i 0) (while (< sumset_i (count sumset_set_a)) (do (set! sumset_j 0) (while (< sumset_j (count sumset_set_b)) (do (set! sumset_s (+ (nth sumset_set_a sumset_i) (nth sumset_set_b sumset_j))) (when (not (contains sumset_result sumset_s)) (set! sumset_result (conj sumset_result sumset_s))) (set! sumset_j (+ sumset_j 1)))) (set! sumset_i (+ sumset_i 1)))) (throw (ex-info "return" {:v sumset_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_set_a nil main_set_b nil main_set_c nil] (do (set! main_set_a [1 2 3]) (set! main_set_b [4 5 6]) (println (str (sumset main_set_a main_set_b))) (set! main_set_c [4 5 6 7]) (println (str (sumset main_set_a main_set_c))))))

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
