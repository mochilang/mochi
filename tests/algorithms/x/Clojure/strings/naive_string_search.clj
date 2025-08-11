(ns main (:refer-clojure :exclude [naive_string_search]))

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

(declare naive_string_search)

(def ^:dynamic naive_string_search_i nil)

(def ^:dynamic naive_string_search_j nil)

(def ^:dynamic naive_string_search_match_found nil)

(def ^:dynamic naive_string_search_pat_len nil)

(def ^:dynamic naive_string_search_positions nil)

(defn naive_string_search [naive_string_search_text naive_string_search_pattern]
  (binding [naive_string_search_i nil naive_string_search_j nil naive_string_search_match_found nil naive_string_search_pat_len nil naive_string_search_positions nil] (try (do (set! naive_string_search_pat_len (count naive_string_search_pattern)) (set! naive_string_search_positions []) (set! naive_string_search_i 0) (while (<= naive_string_search_i (- (count naive_string_search_text) naive_string_search_pat_len)) (do (set! naive_string_search_match_found true) (set! naive_string_search_j 0) (loop [while_flag_1 true] (when (and while_flag_1 (< naive_string_search_j naive_string_search_pat_len)) (cond (not= (subs naive_string_search_text (+ naive_string_search_i naive_string_search_j) (+ (+ naive_string_search_i naive_string_search_j) 1)) (subs naive_string_search_pattern naive_string_search_j (+ naive_string_search_j 1))) (do (set! naive_string_search_match_found false) (recur false)) :else (do (set! naive_string_search_j (+ naive_string_search_j 1)) (recur while_flag_1))))) (when naive_string_search_match_found (set! naive_string_search_positions (conj naive_string_search_positions naive_string_search_i))) (set! naive_string_search_i (+ naive_string_search_i 1)))) (throw (ex-info "return" {:v naive_string_search_positions}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (naive_string_search "ABAAABCDBBABCDDEBCABC" "ABC"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
