(ns main (:refer-clojure :exclude [min_int max_int repeat_bool set_bool jaro_winkler]))

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

(declare min_int max_int repeat_bool set_bool jaro_winkler)

(def ^:dynamic jaro_winkler_end nil)

(def ^:dynamic jaro_winkler_i nil)

(def ^:dynamic jaro_winkler_j nil)

(def ^:dynamic jaro_winkler_jaro nil)

(def ^:dynamic jaro_winkler_k nil)

(def ^:dynamic jaro_winkler_len1 nil)

(def ^:dynamic jaro_winkler_len2 nil)

(def ^:dynamic jaro_winkler_limit nil)

(def ^:dynamic jaro_winkler_m nil)

(def ^:dynamic jaro_winkler_match1 nil)

(def ^:dynamic jaro_winkler_match2 nil)

(def ^:dynamic jaro_winkler_matches nil)

(def ^:dynamic jaro_winkler_prefix_len nil)

(def ^:dynamic jaro_winkler_start nil)

(def ^:dynamic jaro_winkler_transpositions nil)

(def ^:dynamic repeat_bool_i nil)

(def ^:dynamic repeat_bool_res nil)

(def ^:dynamic set_bool_i nil)

(def ^:dynamic set_bool_res nil)

(defn min_int [min_int_a min_int_b]
  (try (if (< min_int_a min_int_b) (throw (ex-info "return" {:v min_int_a})) (throw (ex-info "return" {:v min_int_b}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn max_int [max_int_a max_int_b]
  (try (if (> max_int_a max_int_b) (throw (ex-info "return" {:v max_int_a})) (throw (ex-info "return" {:v max_int_b}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn repeat_bool [repeat_bool_n repeat_bool_value]
  (binding [repeat_bool_i nil repeat_bool_res nil] (try (do (set! repeat_bool_res []) (set! repeat_bool_i 0) (while (< repeat_bool_i repeat_bool_n) (do (set! repeat_bool_res (conj repeat_bool_res repeat_bool_value)) (set! repeat_bool_i (+ repeat_bool_i 1)))) (throw (ex-info "return" {:v repeat_bool_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn set_bool [set_bool_xs set_bool_idx set_bool_value]
  (binding [set_bool_i nil set_bool_res nil] (try (do (set! set_bool_res []) (set! set_bool_i 0) (while (< set_bool_i (count set_bool_xs)) (do (if (= set_bool_i set_bool_idx) (set! set_bool_res (conj set_bool_res set_bool_value)) (set! set_bool_res (conj set_bool_res (nth set_bool_xs set_bool_i)))) (set! set_bool_i (+ set_bool_i 1)))) (throw (ex-info "return" {:v set_bool_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn jaro_winkler [jaro_winkler_s1 jaro_winkler_s2]
  (binding [jaro_winkler_end nil jaro_winkler_i nil jaro_winkler_j nil jaro_winkler_jaro nil jaro_winkler_k nil jaro_winkler_len1 nil jaro_winkler_len2 nil jaro_winkler_limit nil jaro_winkler_m nil jaro_winkler_match1 nil jaro_winkler_match2 nil jaro_winkler_matches nil jaro_winkler_prefix_len nil jaro_winkler_start nil jaro_winkler_transpositions nil] (try (do (set! jaro_winkler_len1 (count jaro_winkler_s1)) (set! jaro_winkler_len2 (count jaro_winkler_s2)) (set! jaro_winkler_limit (/ (min_int jaro_winkler_len1 jaro_winkler_len2) 2)) (set! jaro_winkler_match1 (repeat_bool jaro_winkler_len1 false)) (set! jaro_winkler_match2 (repeat_bool jaro_winkler_len2 false)) (set! jaro_winkler_matches 0) (set! jaro_winkler_i 0) (while (< jaro_winkler_i jaro_winkler_len1) (do (set! jaro_winkler_start (max_int 0 (- jaro_winkler_i jaro_winkler_limit))) (set! jaro_winkler_end (min_int (+ (+ jaro_winkler_i jaro_winkler_limit) 1) jaro_winkler_len2)) (set! jaro_winkler_j jaro_winkler_start) (loop [while_flag_1 true] (when (and while_flag_1 (< jaro_winkler_j jaro_winkler_end)) (cond (and (not (nth jaro_winkler_match2 jaro_winkler_j)) (= (subs jaro_winkler_s1 jaro_winkler_i (min (+ jaro_winkler_i 1) (count jaro_winkler_s1))) (subs jaro_winkler_s2 jaro_winkler_j (min (+ jaro_winkler_j 1) (count jaro_winkler_s2))))) (do (set! jaro_winkler_match1 (set_bool jaro_winkler_match1 jaro_winkler_i true)) (set! jaro_winkler_match2 (set_bool jaro_winkler_match2 jaro_winkler_j true)) (set! jaro_winkler_matches (+ jaro_winkler_matches 1)) (recur false)) :else (do (set! jaro_winkler_j (+ jaro_winkler_j 1)) (recur while_flag_1))))) (set! jaro_winkler_i (+ jaro_winkler_i 1)))) (when (= jaro_winkler_matches 0) (throw (ex-info "return" {:v 0.0}))) (set! jaro_winkler_transpositions 0) (set! jaro_winkler_k 0) (set! jaro_winkler_i 0) (while (< jaro_winkler_i jaro_winkler_len1) (do (when (nth jaro_winkler_match1 jaro_winkler_i) (do (while (not (nth jaro_winkler_match2 jaro_winkler_k)) (set! jaro_winkler_k (+ jaro_winkler_k 1))) (when (not= (subs jaro_winkler_s1 jaro_winkler_i (min (+ jaro_winkler_i 1) (count jaro_winkler_s1))) (subs jaro_winkler_s2 jaro_winkler_k (min (+ jaro_winkler_k 1) (count jaro_winkler_s2)))) (set! jaro_winkler_transpositions (+ jaro_winkler_transpositions 1))) (set! jaro_winkler_k (+ jaro_winkler_k 1)))) (set! jaro_winkler_i (+ jaro_winkler_i 1)))) (set! jaro_winkler_m (double jaro_winkler_matches)) (set! jaro_winkler_jaro (/ (+ (+ (/ jaro_winkler_m (double jaro_winkler_len1)) (/ jaro_winkler_m (double jaro_winkler_len2))) (/ (- jaro_winkler_m (/ (double jaro_winkler_transpositions) 2.0)) jaro_winkler_m)) 3.0)) (set! jaro_winkler_prefix_len 0) (set! jaro_winkler_i 0) (loop [while_flag_2 true] (when (and while_flag_2 (and (and (< jaro_winkler_i 4) (< jaro_winkler_i jaro_winkler_len1)) (< jaro_winkler_i jaro_winkler_len2))) (do (if (= (subs jaro_winkler_s1 jaro_winkler_i (min (+ jaro_winkler_i 1) (count jaro_winkler_s1))) (subs jaro_winkler_s2 jaro_winkler_i (min (+ jaro_winkler_i 1) (count jaro_winkler_s2)))) (set! jaro_winkler_prefix_len (+ jaro_winkler_prefix_len 1)) (recur false)) (set! jaro_winkler_i (+ jaro_winkler_i 1)) (cond :else (recur while_flag_2))))) (throw (ex-info "return" {:v (+ jaro_winkler_jaro (* (* 0.1 (double jaro_winkler_prefix_len)) (- 1.0 jaro_winkler_jaro)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (jaro_winkler "hello" "world")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
