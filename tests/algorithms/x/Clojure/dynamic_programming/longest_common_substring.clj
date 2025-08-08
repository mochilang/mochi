(ns main (:refer-clojure :exclude [longest_common_substring]))

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

(declare longest_common_substring)

(def ^:dynamic longest_common_substring_dp nil)

(def ^:dynamic longest_common_substring_end_pos nil)

(def ^:dynamic longest_common_substring_i nil)

(def ^:dynamic longest_common_substring_ii nil)

(def ^:dynamic longest_common_substring_j nil)

(def ^:dynamic longest_common_substring_jj nil)

(def ^:dynamic longest_common_substring_m nil)

(def ^:dynamic longest_common_substring_max_len nil)

(def ^:dynamic longest_common_substring_n nil)

(def ^:dynamic longest_common_substring_row nil)

(defn longest_common_substring [longest_common_substring_text1 longest_common_substring_text2]
  (binding [longest_common_substring_dp nil longest_common_substring_end_pos nil longest_common_substring_i nil longest_common_substring_ii nil longest_common_substring_j nil longest_common_substring_jj nil longest_common_substring_m nil longest_common_substring_max_len nil longest_common_substring_n nil longest_common_substring_row nil] (try (do (when (or (= (count longest_common_substring_text1) 0) (= (count longest_common_substring_text2) 0)) (throw (ex-info "return" {:v ""}))) (set! longest_common_substring_m (count longest_common_substring_text1)) (set! longest_common_substring_n (count longest_common_substring_text2)) (set! longest_common_substring_dp []) (set! longest_common_substring_i 0) (while (< longest_common_substring_i (+ longest_common_substring_m 1)) (do (set! longest_common_substring_row []) (set! longest_common_substring_j 0) (while (< longest_common_substring_j (+ longest_common_substring_n 1)) (do (set! longest_common_substring_row (conj longest_common_substring_row 0)) (set! longest_common_substring_j (+ longest_common_substring_j 1)))) (set! longest_common_substring_dp (conj longest_common_substring_dp longest_common_substring_row)) (set! longest_common_substring_i (+ longest_common_substring_i 1)))) (set! longest_common_substring_end_pos 0) (set! longest_common_substring_max_len 0) (set! longest_common_substring_ii 1) (while (<= longest_common_substring_ii longest_common_substring_m) (do (set! longest_common_substring_jj 1) (while (<= longest_common_substring_jj longest_common_substring_n) (do (when (= (subs longest_common_substring_text1 (- longest_common_substring_ii 1) (min longest_common_substring_ii (count longest_common_substring_text1))) (subs longest_common_substring_text2 (- longest_common_substring_jj 1) (min longest_common_substring_jj (count longest_common_substring_text2)))) (do (set! longest_common_substring_dp (assoc-in longest_common_substring_dp [longest_common_substring_ii longest_common_substring_jj] (+ 1 (nth (nth longest_common_substring_dp (- longest_common_substring_ii 1)) (- longest_common_substring_jj 1))))) (when (> (nth (nth longest_common_substring_dp longest_common_substring_ii) longest_common_substring_jj) longest_common_substring_max_len) (do (set! longest_common_substring_max_len (nth (nth longest_common_substring_dp longest_common_substring_ii) longest_common_substring_jj)) (set! longest_common_substring_end_pos longest_common_substring_ii))))) (set! longest_common_substring_jj (+ longest_common_substring_jj 1)))) (set! longest_common_substring_ii (+ longest_common_substring_ii 1)))) (throw (ex-info "return" {:v (subs longest_common_substring_text1 (- longest_common_substring_end_pos longest_common_substring_max_len) (min longest_common_substring_end_pos (count longest_common_substring_text1)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (longest_common_substring "abcdef" "xabded"))
      (println "\n")
      (println (longest_common_substring "zxabcdezy" "yzabcdezx"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
