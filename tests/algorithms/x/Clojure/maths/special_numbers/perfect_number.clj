(ns main (:refer-clojure :exclude [perfect main]))

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

(declare perfect main)

(def ^:dynamic main_idx nil)

(def ^:dynamic main_num nil)

(def ^:dynamic main_numbers nil)

(def ^:dynamic perfect_i nil)

(def ^:dynamic perfect_limit nil)

(def ^:dynamic perfect_sum nil)

(defn perfect [perfect_n]
  (binding [perfect_i nil perfect_limit nil perfect_sum nil] (try (do (when (<= perfect_n 0) (throw (ex-info "return" {:v false}))) (set! perfect_limit (quot perfect_n 2)) (set! perfect_sum 0) (set! perfect_i 1) (while (<= perfect_i perfect_limit) (do (when (= (mod perfect_n perfect_i) 0) (set! perfect_sum (+ perfect_sum perfect_i))) (set! perfect_i (+ perfect_i 1)))) (throw (ex-info "return" {:v (= perfect_sum perfect_n)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_idx nil main_num nil main_numbers nil] (do (set! main_numbers [6 28 29 12 496 8128 0 (- 1)]) (set! main_idx 0) (while (< main_idx (count main_numbers)) (do (set! main_num (nth main_numbers main_idx)) (if (perfect main_num) (println (str (str main_num) " is a Perfect Number.")) (println (str (str main_num) " is not a Perfect Number."))) (set! main_idx (+ main_idx 1)))))))

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
