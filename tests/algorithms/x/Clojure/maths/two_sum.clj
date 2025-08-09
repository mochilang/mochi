(ns main (:refer-clojure :exclude [two_sum]))

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

(declare two_sum)

(def ^:dynamic two_sum_chk_map nil)

(def ^:dynamic two_sum_compl nil)

(def ^:dynamic two_sum_idx nil)

(def ^:dynamic two_sum_val nil)

(defn two_sum [two_sum_nums two_sum_target]
  (binding [two_sum_chk_map nil two_sum_compl nil two_sum_idx nil two_sum_val nil] (try (do (set! two_sum_chk_map {}) (set! two_sum_idx 0) (while (< two_sum_idx (count two_sum_nums)) (do (set! two_sum_val (nth two_sum_nums two_sum_idx)) (set! two_sum_compl (- two_sum_target two_sum_val)) (when (in two_sum_compl two_sum_chk_map) (throw (ex-info "return" {:v [(- (get two_sum_chk_map two_sum_compl) 1) two_sum_idx]}))) (set! two_sum_chk_map (assoc two_sum_chk_map two_sum_val (+ two_sum_idx 1))) (set! two_sum_idx (+ two_sum_idx 1)))) (throw (ex-info "return" {:v []}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (two_sum [2 7 11 15] 9)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
