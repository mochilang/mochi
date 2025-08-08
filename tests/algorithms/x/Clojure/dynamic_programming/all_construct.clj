(ns main (:refer-clojure :exclude [allConstruct]))

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

(declare allConstruct)

(def ^:dynamic allConstruct_base nil)

(def ^:dynamic allConstruct_combination nil)

(def ^:dynamic allConstruct_empty nil)

(def ^:dynamic allConstruct_i nil)

(def ^:dynamic allConstruct_idx nil)

(def ^:dynamic allConstruct_k nil)

(def ^:dynamic allConstruct_m nil)

(def ^:dynamic allConstruct_nextIndex nil)

(def ^:dynamic allConstruct_table nil)

(def ^:dynamic allConstruct_tableSize nil)

(def ^:dynamic allConstruct_w nil)

(def ^:dynamic allConstruct_way nil)

(def ^:dynamic allConstruct_word nil)

(def ^:dynamic allConstruct_wordLen nil)

(defn allConstruct [allConstruct_target allConstruct_wordBank]
  (binding [allConstruct_base nil allConstruct_combination nil allConstruct_empty nil allConstruct_i nil allConstruct_idx nil allConstruct_k nil allConstruct_m nil allConstruct_nextIndex nil allConstruct_table nil allConstruct_tableSize nil allConstruct_w nil allConstruct_way nil allConstruct_word nil allConstruct_wordLen nil] (try (do (set! allConstruct_tableSize (+ (count allConstruct_target) 1)) (set! allConstruct_table []) (set! allConstruct_idx 0) (while (< allConstruct_idx allConstruct_tableSize) (do (set! allConstruct_empty []) (set! allConstruct_table (conj allConstruct_table allConstruct_empty)) (set! allConstruct_idx (+ allConstruct_idx 1)))) (set! allConstruct_base []) (set! allConstruct_table (assoc allConstruct_table 0 [allConstruct_base])) (set! allConstruct_i 0) (while (< allConstruct_i allConstruct_tableSize) (do (when (not= (count (nth allConstruct_table allConstruct_i)) 0) (do (set! allConstruct_w 0) (while (< allConstruct_w (count allConstruct_wordBank)) (do (set! allConstruct_word (nth allConstruct_wordBank allConstruct_w)) (set! allConstruct_wordLen (count allConstruct_word)) (when (= (subs allConstruct_target allConstruct_i (min (+ allConstruct_i allConstruct_wordLen) (count allConstruct_target))) allConstruct_word) (do (set! allConstruct_k 0) (while (< allConstruct_k (count (nth allConstruct_table allConstruct_i))) (do (set! allConstruct_way (nth (nth allConstruct_table allConstruct_i) allConstruct_k)) (set! allConstruct_combination []) (set! allConstruct_m 0) (while (< allConstruct_m (count allConstruct_way)) (do (set! allConstruct_combination (conj allConstruct_combination (nth allConstruct_way allConstruct_m))) (set! allConstruct_m (+ allConstruct_m 1)))) (set! allConstruct_combination (conj allConstruct_combination allConstruct_word)) (set! allConstruct_nextIndex (+ allConstruct_i allConstruct_wordLen)) (set! allConstruct_table (assoc allConstruct_table allConstruct_nextIndex (conj (nth allConstruct_table allConstruct_nextIndex) allConstruct_combination))) (set! allConstruct_k (+ allConstruct_k 1)))))) (set! allConstruct_w (+ allConstruct_w 1)))))) (set! allConstruct_i (+ allConstruct_i 1)))) (throw (ex-info "return" {:v (nth allConstruct_table (count allConstruct_target))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (allConstruct "jwajalapa" ["jwa" "j" "w" "a" "la" "lapa"])))
      (println (str (allConstruct "rajamati" ["s" "raj" "amat" "raja" "ma" "i" "t"])))
      (println (str (allConstruct "hexagonosaurus" ["h" "ex" "hex" "ag" "ago" "ru" "auru" "rus" "go" "no" "o" "s"])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
