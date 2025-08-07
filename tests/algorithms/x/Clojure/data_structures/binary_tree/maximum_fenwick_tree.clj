(ns main (:refer-clojure :exclude [zeros update query]))

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

(declare zeros update query)

(def ^:dynamic main_arr nil)

(def ^:dynamic query_i nil)

(def ^:dynamic query_result nil)

(def ^:dynamic update_arr nil)

(def ^:dynamic zeros_i nil)

(def ^:dynamic zeros_res nil)

(defn zeros [zeros_n]
  (binding [zeros_i nil zeros_res nil] (try (do (set! zeros_res []) (set! zeros_i 0) (while (< zeros_i zeros_n) (do (set! zeros_res (conj zeros_res 0)) (set! zeros_i (+ zeros_i 1)))) (throw (ex-info "return" {:v zeros_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn update [update_arr_p update_idx update_value]
  (binding [update_arr nil] (do (set! update_arr update_arr_p) (set! update_arr (assoc update_arr update_idx update_value)))))

(defn query [query_arr query_left query_right]
  (binding [query_i nil query_result nil] (try (do (set! query_result 0) (set! query_i query_left) (while (< query_i query_right) (do (when (> (nth query_arr query_i) query_result) (set! query_result (nth query_arr query_i))) (set! query_i (+ query_i 1)))) (throw (ex-info "return" {:v query_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_arr [0 0 0 0 0])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (query main_arr 0 5))
      (update main_arr 4 100)
      (println (query main_arr 0 5))
      (update main_arr 4 0)
      (update main_arr 2 20)
      (println (query main_arr 0 5))
      (update main_arr 4 10)
      (println (query main_arr 2 5))
      (println (query main_arr 1 5))
      (update main_arr 2 0)
      (println (query main_arr 0 5))
      (def main_arr (zeros 10000))
      (update main_arr 255 30)
      (println (query main_arr 0 10000))
      (def main_arr (zeros 6))
      (update main_arr 5 1)
      (println (query main_arr 5 6))
      (def main_arr (zeros 6))
      (update main_arr 0 1000)
      (println (query main_arr 0 1))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
