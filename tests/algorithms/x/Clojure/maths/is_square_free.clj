(ns main (:refer-clojure :exclude [is_square_free]))

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

(declare is_square_free)

(def ^:dynamic is_square_free_i nil)

(def ^:dynamic is_square_free_j nil)

(defn is_square_free [is_square_free_factors]
  (binding [is_square_free_i nil is_square_free_j nil] (try (do (set! is_square_free_i 0) (while (< is_square_free_i (count is_square_free_factors)) (do (set! is_square_free_j (+ is_square_free_i 1)) (while (< is_square_free_j (count is_square_free_factors)) (do (when (= (nth is_square_free_factors is_square_free_i) (nth is_square_free_factors is_square_free_j)) (throw (ex-info "return" {:v false}))) (set! is_square_free_j (+ is_square_free_j 1)))) (set! is_square_free_i (+ is_square_free_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (is_square_free [1 2 3 4])))
      (println (str (is_square_free [1 1 2 3 4])))
      (println (str (is_square_free [1 2 2 5])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
