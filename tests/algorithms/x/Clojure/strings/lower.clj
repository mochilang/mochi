(ns main (:refer-clojure :exclude [index_of lower]))

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

(declare index_of lower)

(def ^:dynamic index_of_i nil)

(def ^:dynamic lower_c nil)

(def ^:dynamic lower_i nil)

(def ^:dynamic lower_idx nil)

(def ^:dynamic lower_lower_chars nil)

(def ^:dynamic lower_result nil)

(def ^:dynamic lower_upper nil)

(defn index_of [index_of_s index_of_ch]
  (binding [index_of_i nil] (try (do (set! index_of_i 0) (while (< index_of_i (count index_of_s)) (do (when (= (subs index_of_s index_of_i (+ index_of_i 1)) index_of_ch) (throw (ex-info "return" {:v index_of_i}))) (set! index_of_i (+ index_of_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn lower [lower_word]
  (binding [lower_c nil lower_i nil lower_idx nil lower_lower_chars nil lower_result nil lower_upper nil] (try (do (set! lower_upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! lower_lower_chars "abcdefghijklmnopqrstuvwxyz") (set! lower_result "") (set! lower_i 0) (while (< lower_i (count lower_word)) (do (set! lower_c (subs lower_word lower_i (+ lower_i 1))) (set! lower_idx (index_of lower_upper lower_c)) (if (>= lower_idx 0) (set! lower_result (str lower_result (subs lower_lower_chars lower_idx (min (+ lower_idx 1) (count lower_lower_chars))))) (set! lower_result (str lower_result lower_c))) (set! lower_i (+ lower_i 1)))) (throw (ex-info "return" {:v lower_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (lower "wow"))
      (println (lower "HellZo"))
      (println (lower "WHAT"))
      (println (lower "wh[]32"))
      (println (lower "whAT"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
