(ns main (:refer-clojure :exclude [etaoin_index get_letter_count get_frequency_order english_freq_match_score main]))

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

(declare etaoin_index get_letter_count get_frequency_order english_freq_match_score main)

(def ^:dynamic english_freq_match_score_bottom nil)

(def ^:dynamic english_freq_match_score_c nil)

(def ^:dynamic english_freq_match_score_freq_order nil)

(def ^:dynamic english_freq_match_score_i nil)

(def ^:dynamic english_freq_match_score_j nil)

(def ^:dynamic english_freq_match_score_score nil)

(def ^:dynamic english_freq_match_score_top nil)

(def ^:dynamic etaoin_index_i nil)

(def ^:dynamic get_frequency_order_a nil)

(def ^:dynamic get_frequency_order_b nil)

(def ^:dynamic get_frequency_order_f nil)

(def ^:dynamic get_frequency_order_freq nil)

(def ^:dynamic get_frequency_order_g nil)

(def ^:dynamic get_frequency_order_g1 nil)

(def ^:dynamic get_frequency_order_g2 nil)

(def ^:dynamic get_frequency_order_g_len nil)

(def ^:dynamic get_frequency_order_group nil)

(def ^:dynamic get_frequency_order_i nil)

(def ^:dynamic get_frequency_order_idx1 nil)

(def ^:dynamic get_frequency_order_idx2 nil)

(def ^:dynamic get_frequency_order_j nil)

(def ^:dynamic get_frequency_order_letter nil)

(def ^:dynamic get_frequency_order_letter_to_freq nil)

(def ^:dynamic get_frequency_order_max_freq nil)

(def ^:dynamic get_frequency_order_result nil)

(def ^:dynamic get_frequency_order_tmp nil)

(def ^:dynamic get_letter_count_c nil)

(def ^:dynamic get_letter_count_ch nil)

(def ^:dynamic get_letter_count_i nil)

(def ^:dynamic get_letter_count_j nil)

(def ^:dynamic get_letter_count_letter_count nil)

(def ^:dynamic get_letter_count_msg nil)

(def ^:dynamic main_ETAOIN "ETAOINSHRDLCUMWFGYPBVKJXQZ")

(def ^:dynamic main_LETTERS "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn etaoin_index [etaoin_index_letter]
  (binding [etaoin_index_i nil] (try (do (set! etaoin_index_i 0) (while (< etaoin_index_i (count main_ETAOIN)) (do (when (= (subs main_ETAOIN etaoin_index_i (min (+ etaoin_index_i 1) (count main_ETAOIN))) etaoin_index_letter) (throw (ex-info "return" {:v etaoin_index_i}))) (set! etaoin_index_i (+ etaoin_index_i 1)))) (throw (ex-info "return" {:v (count main_ETAOIN)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_letter_count [get_letter_count_message]
  (binding [get_letter_count_c nil get_letter_count_ch nil get_letter_count_i nil get_letter_count_j nil get_letter_count_letter_count nil get_letter_count_msg nil] (try (do (set! get_letter_count_letter_count {}) (set! get_letter_count_i 0) (while (< get_letter_count_i (count main_LETTERS)) (do (set! get_letter_count_c (subs main_LETTERS get_letter_count_i (min (+ get_letter_count_i 1) (count main_LETTERS)))) (set! get_letter_count_letter_count (assoc get_letter_count_letter_count get_letter_count_c 0)) (set! get_letter_count_i (+ get_letter_count_i 1)))) (set! get_letter_count_msg (clojure.string/upper-case get_letter_count_message)) (set! get_letter_count_j 0) (while (< get_letter_count_j (count get_letter_count_msg)) (do (set! get_letter_count_ch (subs get_letter_count_msg get_letter_count_j (min (+ get_letter_count_j 1) (count get_letter_count_msg)))) (when (in get_letter_count_ch main_LETTERS) (set! get_letter_count_letter_count (assoc get_letter_count_letter_count get_letter_count_ch (+ (get get_letter_count_letter_count get_letter_count_ch) 1)))) (set! get_letter_count_j (+ get_letter_count_j 1)))) (throw (ex-info "return" {:v get_letter_count_letter_count}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_frequency_order [get_frequency_order_message]
  (binding [get_frequency_order_a nil get_frequency_order_b nil get_frequency_order_f nil get_frequency_order_freq nil get_frequency_order_g nil get_frequency_order_g1 nil get_frequency_order_g2 nil get_frequency_order_g_len nil get_frequency_order_group nil get_frequency_order_i nil get_frequency_order_idx1 nil get_frequency_order_idx2 nil get_frequency_order_j nil get_frequency_order_letter nil get_frequency_order_letter_to_freq nil get_frequency_order_max_freq nil get_frequency_order_result nil get_frequency_order_tmp nil] (try (do (set! get_frequency_order_letter_to_freq (get_letter_count get_frequency_order_message)) (set! get_frequency_order_max_freq 0) (set! get_frequency_order_i 0) (while (< get_frequency_order_i (count main_LETTERS)) (do (set! get_frequency_order_letter (subs main_LETTERS get_frequency_order_i (min (+ get_frequency_order_i 1) (count main_LETTERS)))) (set! get_frequency_order_f (get get_frequency_order_letter_to_freq get_frequency_order_letter)) (when (> get_frequency_order_f get_frequency_order_max_freq) (set! get_frequency_order_max_freq get_frequency_order_f)) (set! get_frequency_order_i (+ get_frequency_order_i 1)))) (set! get_frequency_order_result "") (set! get_frequency_order_freq get_frequency_order_max_freq) (while (>= get_frequency_order_freq 0) (do (set! get_frequency_order_group []) (set! get_frequency_order_j 0) (while (< get_frequency_order_j (count main_LETTERS)) (do (set! get_frequency_order_letter (subs main_LETTERS get_frequency_order_j (min (+ get_frequency_order_j 1) (count main_LETTERS)))) (when (= (get get_frequency_order_letter_to_freq get_frequency_order_letter) get_frequency_order_freq) (set! get_frequency_order_group (conj get_frequency_order_group get_frequency_order_letter))) (set! get_frequency_order_j (+ get_frequency_order_j 1)))) (set! get_frequency_order_g_len (count get_frequency_order_group)) (set! get_frequency_order_a 0) (while (< get_frequency_order_a get_frequency_order_g_len) (do (set! get_frequency_order_b 0) (while (< get_frequency_order_b (- (- get_frequency_order_g_len get_frequency_order_a) 1)) (do (set! get_frequency_order_g1 (nth get_frequency_order_group get_frequency_order_b)) (set! get_frequency_order_g2 (nth get_frequency_order_group (+ get_frequency_order_b 1))) (set! get_frequency_order_idx1 (etaoin_index get_frequency_order_g1)) (set! get_frequency_order_idx2 (etaoin_index get_frequency_order_g2)) (when (< get_frequency_order_idx1 get_frequency_order_idx2) (do (set! get_frequency_order_tmp (nth get_frequency_order_group get_frequency_order_b)) (set! get_frequency_order_group (assoc get_frequency_order_group get_frequency_order_b (nth get_frequency_order_group (+ get_frequency_order_b 1)))) (set! get_frequency_order_group (assoc get_frequency_order_group (+ get_frequency_order_b 1) get_frequency_order_tmp)))) (set! get_frequency_order_b (+ get_frequency_order_b 1)))) (set! get_frequency_order_a (+ get_frequency_order_a 1)))) (set! get_frequency_order_g 0) (while (< get_frequency_order_g (count get_frequency_order_group)) (do (set! get_frequency_order_result (str get_frequency_order_result (nth get_frequency_order_group get_frequency_order_g))) (set! get_frequency_order_g (+ get_frequency_order_g 1)))) (set! get_frequency_order_freq (- get_frequency_order_freq 1)))) (throw (ex-info "return" {:v get_frequency_order_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn english_freq_match_score [english_freq_match_score_message]
  (binding [english_freq_match_score_bottom nil english_freq_match_score_c nil english_freq_match_score_freq_order nil english_freq_match_score_i nil english_freq_match_score_j nil english_freq_match_score_score nil english_freq_match_score_top nil] (try (do (set! english_freq_match_score_freq_order (get_frequency_order english_freq_match_score_message)) (set! english_freq_match_score_top (subs english_freq_match_score_freq_order 0 (min 6 (count english_freq_match_score_freq_order)))) (set! english_freq_match_score_bottom (subs english_freq_match_score_freq_order (- (count english_freq_match_score_freq_order) 6) (min (count english_freq_match_score_freq_order) (count english_freq_match_score_freq_order)))) (set! english_freq_match_score_score 0) (set! english_freq_match_score_i 0) (while (< english_freq_match_score_i 6) (do (set! english_freq_match_score_c (subs main_ETAOIN english_freq_match_score_i (min (+ english_freq_match_score_i 1) (count main_ETAOIN)))) (when (in english_freq_match_score_c english_freq_match_score_top) (set! english_freq_match_score_score (+ english_freq_match_score_score 1))) (set! english_freq_match_score_i (+ english_freq_match_score_i 1)))) (set! english_freq_match_score_j (- (count main_ETAOIN) 6)) (while (< english_freq_match_score_j (count main_ETAOIN)) (do (set! english_freq_match_score_c (subs main_ETAOIN english_freq_match_score_j (min (+ english_freq_match_score_j 1) (count main_ETAOIN)))) (when (in english_freq_match_score_c english_freq_match_score_bottom) (set! english_freq_match_score_score (+ english_freq_match_score_score 1))) (set! english_freq_match_score_j (+ english_freq_match_score_j 1)))) (throw (ex-info "return" {:v english_freq_match_score_score}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (get_frequency_order "Hello World")) (println (english_freq_match_score "Hello World"))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (main)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
