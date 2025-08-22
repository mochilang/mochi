(ns main (:refer-clojure :exclude [split card_value parse_hand compare main]))

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
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare split card_value parse_hand compare main)

(declare _read_file)

(def ^:dynamic compare_i nil)

(def ^:dynamic main_res nil)

(def ^:dynamic main_t nil)

(def ^:dynamic main_tests nil)

(def ^:dynamic parse_hand_c nil)

(def ^:dynamic parse_hand_card nil)

(def ^:dynamic parse_hand_counts nil)

(def ^:dynamic parse_hand_four_val nil)

(def ^:dynamic parse_hand_i nil)

(def ^:dynamic parse_hand_is_flush nil)

(def ^:dynamic parse_hand_is_straight nil)

(def ^:dynamic parse_hand_j nil)

(def ^:dynamic parse_hand_k nil)

(def ^:dynamic parse_hand_pair_vals nil)

(def ^:dynamic parse_hand_rank nil)

(def ^:dynamic parse_hand_s0 nil)

(def ^:dynamic parse_hand_suits nil)

(def ^:dynamic parse_hand_t nil)

(def ^:dynamic parse_hand_three_val nil)

(def ^:dynamic parse_hand_v nil)

(def ^:dynamic parse_hand_vals nil)

(def ^:dynamic split_cur nil)

(def ^:dynamic split_i nil)

(def ^:dynamic split_parts nil)

(defn split [split_s split_sep]
  (binding [split_cur nil split_i nil split_parts nil] (try (do (set! split_parts []) (set! split_cur "") (set! split_i 0) (while (< split_i (count split_s)) (if (and (and (> (count split_sep) 0) (<= (+ split_i (count split_sep)) (count split_s))) (= (subs split_s split_i (min (+ split_i (count split_sep)) (count split_s))) split_sep)) (do (set! split_parts (conj split_parts split_cur)) (set! split_cur "") (set! split_i (+ split_i (count split_sep)))) (do (set! split_cur (str split_cur (subs split_s split_i (min (+ split_i 1) (count split_s))))) (set! split_i (+ split_i 1))))) (set! split_parts (conj split_parts split_cur)) (throw (ex-info "return" {:v split_parts}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn card_value [card_value_ch]
  (try (if (= card_value_ch "A") (throw (ex-info "return" {:v 14})) (if (= card_value_ch "K") (throw (ex-info "return" {:v 13})) (if (= card_value_ch "Q") (throw (ex-info "return" {:v 12})) (if (= card_value_ch "J") (throw (ex-info "return" {:v 11})) (if (= card_value_ch "T") (throw (ex-info "return" {:v 10})) (if (= card_value_ch "9") (throw (ex-info "return" {:v 9})) (if (= card_value_ch "8") (throw (ex-info "return" {:v 8})) (if (= card_value_ch "7") (throw (ex-info "return" {:v 7})) (if (= card_value_ch "6") (throw (ex-info "return" {:v 6})) (if (= card_value_ch "5") (throw (ex-info "return" {:v 5})) (if (= card_value_ch "4") (throw (ex-info "return" {:v 4})) (if (= card_value_ch "3") (throw (ex-info "return" {:v 3})) (throw (ex-info "return" {:v 2})))))))))))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn parse_hand [parse_hand_hand]
  (binding [parse_hand_c nil parse_hand_card nil parse_hand_counts nil parse_hand_four_val nil parse_hand_i nil parse_hand_is_flush nil parse_hand_is_straight nil parse_hand_j nil parse_hand_k nil parse_hand_pair_vals nil parse_hand_rank nil parse_hand_s0 nil parse_hand_suits nil parse_hand_t nil parse_hand_three_val nil parse_hand_v nil parse_hand_vals nil] (try (do (set! parse_hand_counts []) (set! parse_hand_i 0) (while (<= parse_hand_i 14) (do (set! parse_hand_counts (conj parse_hand_counts 0)) (set! parse_hand_i (+ parse_hand_i 1)))) (set! parse_hand_suits []) (doseq [parse_hand_card (split parse_hand_hand " ")] (do (set! parse_hand_v (card_value (subvec parse_hand_card 0 1))) (set! parse_hand_counts (assoc parse_hand_counts parse_hand_v (+ (nth parse_hand_counts parse_hand_v) 1))) (set! parse_hand_suits (conj parse_hand_suits (subvec parse_hand_card 1 2))))) (set! parse_hand_vals []) (set! parse_hand_v 14) (while (>= parse_hand_v 2) (do (set! parse_hand_c (nth parse_hand_counts parse_hand_v)) (set! parse_hand_k 0) (while (< parse_hand_k parse_hand_c) (do (set! parse_hand_vals (conj parse_hand_vals parse_hand_v)) (set! parse_hand_k (+ parse_hand_k 1)))) (set! parse_hand_v (- parse_hand_v 1)))) (set! parse_hand_is_straight false) (if (and (and (and (and (and (= (count parse_hand_vals) 5) (= (nth parse_hand_vals 0) 14)) (= (nth parse_hand_vals 1) 5)) (= (nth parse_hand_vals 2) 4)) (= (nth parse_hand_vals 3) 3)) (= (nth parse_hand_vals 4) 2)) (do (set! parse_hand_is_straight true) (set! parse_hand_vals (assoc parse_hand_vals 0 5)) (set! parse_hand_vals (assoc parse_hand_vals 1 4)) (set! parse_hand_vals (assoc parse_hand_vals 2 3)) (set! parse_hand_vals (assoc parse_hand_vals 3 2)) (set! parse_hand_vals (assoc parse_hand_vals 4 14))) (do (set! parse_hand_is_straight true) (set! parse_hand_j 0) (while (< parse_hand_j 4) (do (when (not= (- (nth parse_hand_vals parse_hand_j) (nth parse_hand_vals (+ parse_hand_j 1))) 1) (set! parse_hand_is_straight false)) (set! parse_hand_j (+ parse_hand_j 1)))))) (set! parse_hand_is_flush true) (set! parse_hand_s0 (nth parse_hand_suits 0)) (set! parse_hand_t 1) (while (< parse_hand_t (count parse_hand_suits)) (do (when (not= (nth parse_hand_suits parse_hand_t) parse_hand_s0) (set! parse_hand_is_flush false)) (set! parse_hand_t (+ parse_hand_t 1)))) (set! parse_hand_four_val 0) (set! parse_hand_three_val 0) (set! parse_hand_pair_vals []) (set! parse_hand_v 14) (while (>= parse_hand_v 2) (do (if (= (nth parse_hand_counts parse_hand_v) 4) (set! parse_hand_four_val parse_hand_v) (if (= (nth parse_hand_counts parse_hand_v) 3) (set! parse_hand_three_val parse_hand_v) (when (= (nth parse_hand_counts parse_hand_v) 2) (set! parse_hand_pair_vals (conj parse_hand_pair_vals parse_hand_v))))) (set! parse_hand_v (- parse_hand_v 1)))) (set! parse_hand_rank 1) (if (and (and (and parse_hand_is_flush parse_hand_is_straight) (= (nth parse_hand_vals 0) 14)) (= (nth parse_hand_vals 4) 10)) (set! parse_hand_rank 10) (if (and parse_hand_is_flush parse_hand_is_straight) (set! parse_hand_rank 9) (if (not= parse_hand_four_val 0) (set! parse_hand_rank 8) (if (and (not= parse_hand_three_val 0) (= (count parse_hand_pair_vals) 1)) (set! parse_hand_rank 7) (if parse_hand_is_flush (set! parse_hand_rank 6) (if parse_hand_is_straight (set! parse_hand_rank 5) (if (not= parse_hand_three_val 0) (set! parse_hand_rank 4) (if (= (count parse_hand_pair_vals) 2) (set! parse_hand_rank 3) (if (= (count parse_hand_pair_vals) 1) (set! parse_hand_rank 2) (set! parse_hand_rank 1)))))))))) (throw (ex-info "return" {:v {:rank parse_hand_rank :values parse_hand_vals}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn compare [compare_a compare_b]
  (binding [compare_i nil] (try (do (when (> (:rank compare_a) (:rank compare_b)) (throw (ex-info "return" {:v "Win"}))) (when (< (:rank compare_a) (:rank compare_b)) (throw (ex-info "return" {:v "Loss"}))) (set! compare_i 0) (while (< compare_i (count (:values compare_a))) (do (when (> (get (:values compare_a) compare_i) (get (:values compare_b) compare_i)) (throw (ex-info "return" {:v "Win"}))) (when (< (get (:values compare_a) compare_i) (get (:values compare_b) compare_i)) (throw (ex-info "return" {:v "Loss"}))) (set! compare_i (+ compare_i 1)))) (throw (ex-info "return" {:v "Tie"}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_res nil main_t nil main_tests nil] (do (set! main_tests [["2H 3H 4H 5H 6H" "KS AS TS QS JS" "Loss"] ["2H 3H 4H 5H 6H" "AS AD AC AH JD" "Win"] ["AS AH 2H AD AC" "JS JD JC JH 3D" "Win"] ["2S AH 2H AS AC" "JS JD JC JH AD" "Loss"] ["2S AH 2H AS AC" "2H 3H 5H 6H 7H" "Win"]]) (doseq [main_t main_tests] (do (set! main_res (compare (parse_hand (nth main_t 0)) (parse_hand (nth main_t 1)))) (println (str (str main_res " expected ") (nth main_t 2))))))))

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
