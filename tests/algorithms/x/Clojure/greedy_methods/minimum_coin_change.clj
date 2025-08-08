(ns main (:refer-clojure :exclude [find_minimum_change]))

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

(declare find_minimum_change)

(def ^:dynamic find_minimum_change_answer nil)

(def ^:dynamic find_minimum_change_denom nil)

(def ^:dynamic find_minimum_change_i nil)

(def ^:dynamic find_minimum_change_total nil)

(defn find_minimum_change [find_minimum_change_denominations find_minimum_change_value]
  (binding [find_minimum_change_answer nil find_minimum_change_denom nil find_minimum_change_i nil find_minimum_change_total nil] (try (do (when (<= find_minimum_change_value 0) (throw (ex-info "return" {:v []}))) (set! find_minimum_change_total find_minimum_change_value) (set! find_minimum_change_answer []) (set! find_minimum_change_i (- (count find_minimum_change_denominations) 1)) (while (>= find_minimum_change_i 0) (do (set! find_minimum_change_denom (nth find_minimum_change_denominations find_minimum_change_i)) (while (>= find_minimum_change_total find_minimum_change_denom) (do (set! find_minimum_change_total (- find_minimum_change_total find_minimum_change_denom)) (set! find_minimum_change_answer (conj find_minimum_change_answer find_minimum_change_denom)))) (set! find_minimum_change_i (- find_minimum_change_i 1)))) (throw (ex-info "return" {:v find_minimum_change_answer}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (find_minimum_change [1 2 5 10 20 50 100 500 2000] 987)))
      (println (str (find_minimum_change [1 5 100 500 1000] 456)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
