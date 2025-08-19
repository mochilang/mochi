(ns main (:refer-clojure :exclude [max_int max_subsequence_sum]))

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

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare max_int max_subsequence_sum)

(def ^:dynamic max_subsequence_sum_ans nil)

(def ^:dynamic max_subsequence_sum_extended nil)

(def ^:dynamic max_subsequence_sum_i nil)

(def ^:dynamic max_subsequence_sum_num nil)

(defn max_int [max_int_a max_int_b]
  (try (if (>= max_int_a max_int_b) (throw (ex-info "return" {:v max_int_a})) (throw (ex-info "return" {:v max_int_b}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn max_subsequence_sum [max_subsequence_sum_nums]
  (binding [max_subsequence_sum_ans nil max_subsequence_sum_extended nil max_subsequence_sum_i nil max_subsequence_sum_num nil] (try (do (when (= (count max_subsequence_sum_nums) 0) (throw (Exception. "input sequence should not be empty"))) (set! max_subsequence_sum_ans (nth max_subsequence_sum_nums 0)) (set! max_subsequence_sum_i 1) (while (< max_subsequence_sum_i (count max_subsequence_sum_nums)) (do (set! max_subsequence_sum_num (nth max_subsequence_sum_nums max_subsequence_sum_i)) (set! max_subsequence_sum_extended (+ max_subsequence_sum_ans max_subsequence_sum_num)) (set! max_subsequence_sum_ans (max_int (max_int max_subsequence_sum_ans max_subsequence_sum_extended) max_subsequence_sum_num)) (set! max_subsequence_sum_i (+ max_subsequence_sum_i 1)))) (throw (ex-info "return" {:v max_subsequence_sum_ans}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (max_subsequence_sum [1 2 3 4 (- 2)]))
      (println (max_subsequence_sum [(- 2) (- 3) (- 1) (- 4) (- 6)]))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
