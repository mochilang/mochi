(ns main (:refer-clojure :exclude [recursive_match dp_match print_bool]))

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

(declare recursive_match dp_match print_bool)

(def ^:dynamic dp_match_dp nil)

(def ^:dynamic dp_match_i nil)

(def ^:dynamic dp_match_j nil)

(def ^:dynamic dp_match_m nil)

(def ^:dynamic dp_match_n nil)

(def ^:dynamic dp_match_p_char nil)

(def ^:dynamic dp_match_prev_p nil)

(def ^:dynamic dp_match_row nil)

(def ^:dynamic dp_match_t_char nil)

(def ^:dynamic recursive_match_last_pattern nil)

(def ^:dynamic recursive_match_last_text nil)

(defn recursive_match [recursive_match_text recursive_match_pattern]
  (binding [recursive_match_last_pattern nil recursive_match_last_text nil] (try (do (when (= (count recursive_match_pattern) 0) (throw (ex-info "return" {:v (= (count recursive_match_text) 0)}))) (when (= (count recursive_match_text) 0) (do (when (and (>= (count recursive_match_pattern) 2) (= (subs recursive_match_pattern (- (count recursive_match_pattern) 1) (min (count recursive_match_pattern) (count recursive_match_pattern))) "*")) (throw (ex-info "return" {:v (recursive_match recursive_match_text (subs recursive_match_pattern 0 (min (- (count recursive_match_pattern) 2) (count recursive_match_pattern))))}))) (throw (ex-info "return" {:v false})))) (set! recursive_match_last_text (subs recursive_match_text (- (count recursive_match_text) 1) (min (count recursive_match_text) (count recursive_match_text)))) (set! recursive_match_last_pattern (subs recursive_match_pattern (- (count recursive_match_pattern) 1) (min (count recursive_match_pattern) (count recursive_match_pattern)))) (when (or (= recursive_match_last_text recursive_match_last_pattern) (= recursive_match_last_pattern ".")) (throw (ex-info "return" {:v (recursive_match (subs recursive_match_text 0 (min (- (count recursive_match_text) 1) (count recursive_match_text))) (subs recursive_match_pattern 0 (min (- (count recursive_match_pattern) 1) (count recursive_match_pattern))))}))) (when (= recursive_match_last_pattern "*") (do (when (recursive_match (subs recursive_match_text 0 (min (- (count recursive_match_text) 1) (count recursive_match_text))) recursive_match_pattern) (throw (ex-info "return" {:v true}))) (throw (ex-info "return" {:v (recursive_match recursive_match_text (subs recursive_match_pattern 0 (min (- (count recursive_match_pattern) 2) (count recursive_match_pattern))))})))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dp_match [dp_match_text dp_match_pattern]
  (binding [dp_match_dp nil dp_match_i nil dp_match_j nil dp_match_m nil dp_match_n nil dp_match_p_char nil dp_match_prev_p nil dp_match_row nil dp_match_t_char nil] (try (do (set! dp_match_m (count dp_match_text)) (set! dp_match_n (count dp_match_pattern)) (set! dp_match_dp []) (set! dp_match_i 0) (while (<= dp_match_i dp_match_m) (do (set! dp_match_row []) (set! dp_match_j 0) (while (<= dp_match_j dp_match_n) (do (set! dp_match_row (conj dp_match_row false)) (set! dp_match_j (+ dp_match_j 1)))) (set! dp_match_dp (conj dp_match_dp dp_match_row)) (set! dp_match_i (+ dp_match_i 1)))) (set! dp_match_dp (assoc-in dp_match_dp [0 0] true)) (set! dp_match_j 1) (while (<= dp_match_j dp_match_n) (do (when (and (= (subs dp_match_pattern (- dp_match_j 1) (min dp_match_j (count dp_match_pattern))) "*") (>= dp_match_j 2)) (when (nth (nth dp_match_dp 0) (- dp_match_j 2)) (set! dp_match_dp (assoc-in dp_match_dp [0 dp_match_j] true)))) (set! dp_match_j (+ dp_match_j 1)))) (set! dp_match_i 1) (while (<= dp_match_i dp_match_m) (do (set! dp_match_j 1) (while (<= dp_match_j dp_match_n) (do (set! dp_match_p_char (subs dp_match_pattern (- dp_match_j 1) (min dp_match_j (count dp_match_pattern)))) (set! dp_match_t_char (subs dp_match_text (- dp_match_i 1) (min dp_match_i (count dp_match_text)))) (if (or (= dp_match_p_char ".") (= dp_match_p_char dp_match_t_char)) (when (nth (nth dp_match_dp (- dp_match_i 1)) (- dp_match_j 1)) (set! dp_match_dp (assoc-in dp_match_dp [dp_match_i dp_match_j] true))) (if (= dp_match_p_char "*") (when (>= dp_match_j 2) (do (when (nth (nth dp_match_dp dp_match_i) (- dp_match_j 2)) (set! dp_match_dp (assoc-in dp_match_dp [dp_match_i dp_match_j] true))) (set! dp_match_prev_p (subs dp_match_pattern (- dp_match_j 2) (min (- dp_match_j 1) (count dp_match_pattern)))) (when (or (= dp_match_prev_p ".") (= dp_match_prev_p dp_match_t_char)) (when (nth (nth dp_match_dp (- dp_match_i 1)) dp_match_j) (set! dp_match_dp (assoc-in dp_match_dp [dp_match_i dp_match_j] true)))))) (set! dp_match_dp (assoc-in dp_match_dp [dp_match_i dp_match_j] false)))) (set! dp_match_j (+ dp_match_j 1)))) (set! dp_match_i (+ dp_match_i 1)))) (throw (ex-info "return" {:v (nth (nth dp_match_dp dp_match_m) dp_match_n)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_bool [print_bool_b]
  (if print_bool_b (println true) (println false)))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (print_bool (recursive_match "abc" "a.c"))
      (print_bool (recursive_match "abc" "af*.c"))
      (print_bool (recursive_match "abc" "a.c*"))
      (print_bool (recursive_match "abc" "a.c*d"))
      (print_bool (recursive_match "aa" ".*"))
      (print_bool (dp_match "abc" "a.c"))
      (print_bool (dp_match "abc" "af*.c"))
      (print_bool (dp_match "abc" "a.c*"))
      (print_bool (dp_match "abc" "a.c*d"))
      (print_bool (dp_match "aa" ".*"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
