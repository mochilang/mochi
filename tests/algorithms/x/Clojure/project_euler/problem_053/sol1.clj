(ns main (:refer-clojure :exclude [combination_exceeds count_exceeding]))

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
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare combination_exceeds count_exceeding)

(declare _read_file)

(def ^:dynamic combination_exceeds_k nil)

(def ^:dynamic combination_exceeds_r2 nil)

(def ^:dynamic combination_exceeds_result nil)

(def ^:dynamic count_exceeding_n nil)

(def ^:dynamic count_exceeding_r nil)

(def ^:dynamic count_exceeding_total nil)

(defn combination_exceeds [combination_exceeds_n combination_exceeds_r combination_exceeds_limit]
  (binding [combination_exceeds_k nil combination_exceeds_r2 nil combination_exceeds_result nil] (try (do (set! combination_exceeds_r2 combination_exceeds_r) (when (> combination_exceeds_r2 (- combination_exceeds_n combination_exceeds_r2)) (set! combination_exceeds_r2 (- combination_exceeds_n combination_exceeds_r2))) (set! combination_exceeds_result 1) (set! combination_exceeds_k 1) (while (<= combination_exceeds_k combination_exceeds_r2) (do (set! combination_exceeds_result (/ (* combination_exceeds_result (+ (- combination_exceeds_n combination_exceeds_r2) combination_exceeds_k)) combination_exceeds_k)) (when (> combination_exceeds_result combination_exceeds_limit) (throw (ex-info "return" {:v true}))) (set! combination_exceeds_k (+ combination_exceeds_k 1)))) (throw (ex-info "return" {:v (> combination_exceeds_result combination_exceeds_limit)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn count_exceeding [count_exceeding_limit]
  (binding [count_exceeding_n nil count_exceeding_r nil count_exceeding_total nil] (try (do (set! count_exceeding_total 0) (set! count_exceeding_n 1) (while (<= count_exceeding_n 100) (do (set! count_exceeding_r 1) (while (<= count_exceeding_r count_exceeding_n) (do (when (combination_exceeds count_exceeding_n count_exceeding_r count_exceeding_limit) (set! count_exceeding_total (+ count_exceeding_total 1))) (set! count_exceeding_r (+ count_exceeding_r 1)))) (set! count_exceeding_n (+ count_exceeding_n 1)))) (throw (ex-info "return" {:v count_exceeding_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (mochi_str (count_exceeding 1000000)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
