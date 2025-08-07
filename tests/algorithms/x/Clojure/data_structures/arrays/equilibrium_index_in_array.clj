(ns main (:refer-clojure :exclude [equilibrium_index]))

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

(declare equilibrium_index)

(def ^:dynamic equilibrium_index_i nil)

(def ^:dynamic equilibrium_index_left nil)

(def ^:dynamic equilibrium_index_total nil)

(defn equilibrium_index [equilibrium_index_arr]
  (binding [equilibrium_index_i nil equilibrium_index_left nil equilibrium_index_total nil] (try (do (set! equilibrium_index_total 0) (set! equilibrium_index_i 0) (while (< equilibrium_index_i (count equilibrium_index_arr)) (do (set! equilibrium_index_total (+ equilibrium_index_total (nth equilibrium_index_arr equilibrium_index_i))) (set! equilibrium_index_i (+ equilibrium_index_i 1)))) (set! equilibrium_index_left 0) (set! equilibrium_index_i 0) (while (< equilibrium_index_i (count equilibrium_index_arr)) (do (set! equilibrium_index_total (- equilibrium_index_total (nth equilibrium_index_arr equilibrium_index_i))) (when (= equilibrium_index_left equilibrium_index_total) (throw (ex-info "return" {:v equilibrium_index_i}))) (set! equilibrium_index_left (+ equilibrium_index_left (nth equilibrium_index_arr equilibrium_index_i))) (set! equilibrium_index_i (+ equilibrium_index_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_arr1 [(- 7) 1 5 2 (- 4) 3 0])

(def ^:dynamic main_arr2 [1 2 3 4 5])

(def ^:dynamic main_arr3 [1 1 1 1 1])

(def ^:dynamic main_arr4 [2 4 6 8 10 3])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (equilibrium_index main_arr1))
      (println (equilibrium_index main_arr2))
      (println (equilibrium_index main_arr3))
      (println (equilibrium_index main_arr4))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
