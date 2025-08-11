(ns main (:refer-clojure :exclude [index_of upper]))

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

(declare index_of upper)

(def ^:dynamic index_of_i nil)

(def ^:dynamic upper_c nil)

(def ^:dynamic upper_i nil)

(def ^:dynamic upper_idx nil)

(def ^:dynamic upper_lower_chars nil)

(def ^:dynamic upper_result nil)

(def ^:dynamic upper_upper_chars nil)

(defn index_of [index_of_s index_of_ch]
  (binding [index_of_i nil] (try (do (set! index_of_i 0) (while (< index_of_i (count index_of_s)) (do (when (= (subs index_of_s index_of_i (+ index_of_i 1)) index_of_ch) (throw (ex-info "return" {:v index_of_i}))) (set! index_of_i (+ index_of_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn upper [upper_word]
  (binding [upper_c nil upper_i nil upper_idx nil upper_lower_chars nil upper_result nil upper_upper_chars nil] (try (do (set! upper_lower_chars "abcdefghijklmnopqrstuvwxyz") (set! upper_upper_chars "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! upper_result "") (set! upper_i 0) (while (< upper_i (count upper_word)) (do (set! upper_c (subs upper_word upper_i (+ upper_i 1))) (set! upper_idx (index_of upper_lower_chars upper_c)) (if (>= upper_idx 0) (set! upper_result (str upper_result (subs upper_upper_chars upper_idx (min (+ upper_idx 1) (count upper_upper_chars))))) (set! upper_result (str upper_result upper_c))) (set! upper_i (+ upper_i 1)))) (throw (ex-info "return" {:v upper_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (upper "wow"))
      (println (upper "Hello"))
      (println (upper "WHAT"))
      (println (upper "wh[]32"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
