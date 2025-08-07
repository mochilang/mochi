(ns main (:refer-clojure :exclude [peak main]))

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

(declare peak main)

(def ^:dynamic peak_high nil)

(def ^:dynamic peak_low nil)

(def ^:dynamic peak_mid nil)

(defn peak [peak_lst]
  (binding [peak_high nil peak_low nil peak_mid nil] (try (do (set! peak_low 0) (set! peak_high (- (count peak_lst) 1)) (while (< peak_low peak_high) (do (set! peak_mid (quot (+ peak_low peak_high) 2)) (if (< (nth peak_lst peak_mid) (nth peak_lst (+ peak_mid 1))) (set! peak_low (+ peak_mid 1)) (set! peak_high peak_mid)))) (throw (ex-info "return" {:v (nth peak_lst peak_low)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (str (peak [1 2 3 4 5 4 3 2 1]))) (println (str (peak [1 10 9 8 7 6 5 4]))) (println (str (peak [1 9 8 7]))) (println (str (peak [1 2 3 4 5 6 7 0]))) (println (str (peak [1 2 3 4 3 2 1 0 (- 1) (- 2)])))))

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
