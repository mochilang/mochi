(ns main (:refer-clojure :exclude [pad hail main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare pad hail main)

(declare cnts counts done i j jobs k line out res s seqs)

(def jobs 12)

(defn pad [n]
  (try (do (def s (str n)) (while (< (count s) 4) (def s (str " " s))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn hail [seq_p cnt_p]
  (try (do (def seq seq_p) (def cnt cnt_p) (def out (pad seq)) (when (not= seq 1) (do (def cnt (+' cnt 1)) (if (not= (mod seq 2) 0) (def seq (+' (* 3 seq) 1)) (def seq (/ seq 2))))) (throw (ex-info "return" {:v {:seq seq :cnt cnt :out out}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def seqs []) (def cnts []) (dotimes [i jobs] (do (def seqs (conj seqs (+' i 1))) (def cnts (conj cnts 0)))) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (def line "") (def i 0) (while (< i jobs) (do (def res (hail (nth seqs i) (nth cnts i))) (def seqs (assoc seqs i (:seq res))) (def cnts (assoc cnts i (:cnt res))) (def line (str line (:out res))) (def i (+' i 1)))) (println line) (def done true) (def j 0) (while (< j jobs) (do (when (not= (nth seqs j) 1) (def done false)) (def j (+' j 1)))) (cond done (recur false) :else (recur while_flag_1))))) (println "") (println "COUNTS:") (def counts "") (def k 0) (while (< k jobs) (do (def counts (str counts (pad (nth cnts k)))) (def k (+' k 1)))) (println counts) (println "")))

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
