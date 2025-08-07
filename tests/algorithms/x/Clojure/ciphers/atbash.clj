(ns main (:refer-clojure :exclude [index_of atbash]))

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

(declare index_of atbash)

(def ^:dynamic atbash_ch nil)

(def ^:dynamic atbash_i nil)

(def ^:dynamic atbash_idx nil)

(def ^:dynamic atbash_idx2 nil)

(def ^:dynamic atbash_lower nil)

(def ^:dynamic atbash_lower_rev nil)

(def ^:dynamic atbash_result nil)

(def ^:dynamic atbash_upper nil)

(def ^:dynamic atbash_upper_rev nil)

(def ^:dynamic index_of_i nil)

(defn index_of [index_of_s index_of_c]
  (binding [index_of_i nil] (try (do (set! index_of_i 0) (while (< index_of_i (count index_of_s)) (do (when (= (nth index_of_s index_of_i) index_of_c) (throw (ex-info "return" {:v index_of_i}))) (set! index_of_i (+ index_of_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn atbash [atbash_sequence]
  (binding [atbash_ch nil atbash_i nil atbash_idx nil atbash_idx2 nil atbash_lower nil atbash_lower_rev nil atbash_result nil atbash_upper nil atbash_upper_rev nil] (try (do (set! atbash_lower "abcdefghijklmnopqrstuvwxyz") (set! atbash_upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! atbash_lower_rev "zyxwvutsrqponmlkjihgfedcba") (set! atbash_upper_rev "ZYXWVUTSRQPONMLKJIHGFEDCBA") (set! atbash_result "") (set! atbash_i 0) (while (< atbash_i (count atbash_sequence)) (do (set! atbash_ch (nth atbash_sequence atbash_i)) (set! atbash_idx (index_of atbash_lower atbash_ch)) (if (not= atbash_idx (- 1)) (set! atbash_result (str atbash_result (nth atbash_lower_rev atbash_idx))) (do (set! atbash_idx2 (index_of atbash_upper atbash_ch)) (if (not= atbash_idx2 (- 1)) (set! atbash_result (str atbash_result (nth atbash_upper_rev atbash_idx2))) (set! atbash_result (str atbash_result atbash_ch))))) (set! atbash_i (+ atbash_i 1)))) (throw (ex-info "return" {:v atbash_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (atbash "ABCDEFGH"))
      (println (atbash "123GGjj"))
      (println (atbash "testStringtest"))
      (println (atbash "with space"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
