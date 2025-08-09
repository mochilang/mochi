(ns main (:refer-clojure :exclude [log2 analyze_text round_to_int calculate_entropy]))

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

(declare log2 analyze_text round_to_int calculate_entropy)

(def ^:dynamic analyze_text_ch nil)

(def ^:dynamic analyze_text_double nil)

(def ^:dynamic analyze_text_i nil)

(def ^:dynamic analyze_text_last nil)

(def ^:dynamic analyze_text_n nil)

(def ^:dynamic analyze_text_pair0 nil)

(def ^:dynamic analyze_text_seq nil)

(def ^:dynamic analyze_text_single nil)

(def ^:dynamic calculate_entropy_a0 nil)

(def ^:dynamic calculate_entropy_a1 nil)

(def ^:dynamic calculate_entropy_alphas nil)

(def ^:dynamic calculate_entropy_ch nil)

(def ^:dynamic calculate_entropy_ch0 nil)

(def ^:dynamic calculate_entropy_ch1 nil)

(def ^:dynamic calculate_entropy_counts nil)

(def ^:dynamic calculate_entropy_diff nil)

(def ^:dynamic calculate_entropy_first_entropy nil)

(def ^:dynamic calculate_entropy_h1 nil)

(def ^:dynamic calculate_entropy_h2 nil)

(def ^:dynamic calculate_entropy_i nil)

(def ^:dynamic calculate_entropy_prob nil)

(def ^:dynamic calculate_entropy_second_entropy nil)

(def ^:dynamic calculate_entropy_seq nil)

(def ^:dynamic calculate_entropy_total1 nil)

(def ^:dynamic calculate_entropy_total2 nil)

(def ^:dynamic first_v nil)

(def ^:dynamic log2_i nil)

(def ^:dynamic log2_k nil)

(def ^:dynamic log2_ln2 nil)

(def ^:dynamic log2_sum nil)

(def ^:dynamic log2_v nil)

(def ^:dynamic log2_z nil)

(def ^:dynamic log2_zpow nil)

(defn log2 [log2_x]
  (binding [log2_i nil log2_k nil log2_ln2 nil log2_sum nil log2_v nil log2_z nil log2_zpow nil] (try (do (set! log2_k 0.0) (set! log2_v log2_x) (while (>= log2_v 2.0) (do (set! log2_v (/ log2_v 2.0)) (set! log2_k (+ log2_k 1.0)))) (while (< log2_v 1.0) (do (set! log2_v (* log2_v 2.0)) (set! log2_k (- log2_k 1.0)))) (set! log2_z (quot (- log2_v 1.0) (+ log2_v 1.0))) (set! log2_zpow log2_z) (set! log2_sum log2_z) (set! log2_i 3) (while (<= log2_i 9) (do (set! log2_zpow (* (* log2_zpow log2_z) log2_z)) (set! log2_sum (+ log2_sum (quot log2_zpow (double log2_i)))) (set! log2_i (+ log2_i 2)))) (set! log2_ln2 0.6931471805599453) (throw (ex-info "return" {:v (+ log2_k (quot (* 2.0 log2_sum) log2_ln2))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn analyze_text [analyze_text_text]
  (binding [analyze_text_ch nil analyze_text_double nil analyze_text_i nil analyze_text_last nil analyze_text_n nil analyze_text_pair0 nil analyze_text_seq nil analyze_text_single nil first_v nil] (try (do (set! analyze_text_single {}) (set! analyze_text_double {}) (set! analyze_text_n (count analyze_text_text)) (when (= analyze_text_n 0) (throw (ex-info "return" {:v {:double analyze_text_double :single analyze_text_single}}))) (set! analyze_text_last (subs analyze_text_text (- analyze_text_n 1) (min analyze_text_n (count analyze_text_text)))) (if (in analyze_text_last analyze_text_single) (set! analyze_text_single (assoc analyze_text_single analyze_text_last (+ (get analyze_text_single analyze_text_last) 1))) (set! analyze_text_single (assoc analyze_text_single analyze_text_last 1))) (set! first_v (subs analyze_text_text 0 (min 1 (count analyze_text_text)))) (set! analyze_text_pair0 (str " " first_v)) (set! analyze_text_double (assoc analyze_text_double analyze_text_pair0 1)) (set! analyze_text_i 0) (while (< analyze_text_i (- analyze_text_n 1)) (do (set! analyze_text_ch (subs analyze_text_text analyze_text_i (min (+ analyze_text_i 1) (count analyze_text_text)))) (if (in analyze_text_ch analyze_text_single) (set! analyze_text_single (assoc analyze_text_single analyze_text_ch (+ (get analyze_text_single analyze_text_ch) 1))) (set! analyze_text_single (assoc analyze_text_single analyze_text_ch 1))) (set! analyze_text_seq (subs analyze_text_text analyze_text_i (min (+ analyze_text_i 2) (count analyze_text_text)))) (if (in analyze_text_seq analyze_text_double) (set! analyze_text_double (assoc analyze_text_double analyze_text_seq (+ (get analyze_text_double analyze_text_seq) 1))) (set! analyze_text_double (assoc analyze_text_double analyze_text_seq 1))) (set! analyze_text_i (+ analyze_text_i 1)))) (throw (ex-info "return" {:v {:double analyze_text_double :single analyze_text_single}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn round_to_int [round_to_int_x]
  (try (if (< round_to_int_x 0.0) (long (- round_to_int_x 0.5)) (long (+ round_to_int_x 0.5))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn calculate_entropy [calculate_entropy_text]
  (binding [calculate_entropy_a0 nil calculate_entropy_a1 nil calculate_entropy_alphas nil calculate_entropy_ch nil calculate_entropy_ch0 nil calculate_entropy_ch1 nil calculate_entropy_counts nil calculate_entropy_diff nil calculate_entropy_first_entropy nil calculate_entropy_h1 nil calculate_entropy_h2 nil calculate_entropy_i nil calculate_entropy_prob nil calculate_entropy_second_entropy nil calculate_entropy_seq nil calculate_entropy_total1 nil calculate_entropy_total2 nil] (do (set! calculate_entropy_counts (analyze_text calculate_entropy_text)) (set! calculate_entropy_alphas " abcdefghijklmnopqrstuvwxyz") (set! calculate_entropy_total1 0) (doseq [ch (keys (:single calculate_entropy_counts))] (set! calculate_entropy_total1 (+ calculate_entropy_total1 (get (:single calculate_entropy_counts) ch)))) (set! calculate_entropy_h1 0.0) (set! calculate_entropy_i 0) (while (< calculate_entropy_i (count calculate_entropy_alphas)) (do (set! calculate_entropy_ch (subs calculate_entropy_alphas calculate_entropy_i (min (+ calculate_entropy_i 1) (count calculate_entropy_alphas)))) (when (in calculate_entropy_ch (:single calculate_entropy_counts)) (do (set! calculate_entropy_prob (quot (double (get (:single calculate_entropy_counts) calculate_entropy_ch)) (double calculate_entropy_total1))) (set! calculate_entropy_h1 (+ calculate_entropy_h1 (* calculate_entropy_prob (log2 calculate_entropy_prob)))))) (set! calculate_entropy_i (+ calculate_entropy_i 1)))) (set! calculate_entropy_first_entropy (- calculate_entropy_h1)) (println (str (str (round_to_int calculate_entropy_first_entropy)) ".0")) (set! calculate_entropy_total2 0) (doseq [seq (keys (:double calculate_entropy_counts))] (set! calculate_entropy_total2 (+ calculate_entropy_total2 (get (:double calculate_entropy_counts) seq)))) (set! calculate_entropy_h2 0.0) (set! calculate_entropy_a0 0) (while (< calculate_entropy_a0 (count calculate_entropy_alphas)) (do (set! calculate_entropy_ch0 (subs calculate_entropy_alphas calculate_entropy_a0 (min (+ calculate_entropy_a0 1) (count calculate_entropy_alphas)))) (set! calculate_entropy_a1 0) (while (< calculate_entropy_a1 (count calculate_entropy_alphas)) (do (set! calculate_entropy_ch1 (subs calculate_entropy_alphas calculate_entropy_a1 (min (+ calculate_entropy_a1 1) (count calculate_entropy_alphas)))) (set! calculate_entropy_seq (str calculate_entropy_ch0 calculate_entropy_ch1)) (when (in calculate_entropy_seq (:double calculate_entropy_counts)) (do (set! calculate_entropy_prob (quot (double (get (:double calculate_entropy_counts) calculate_entropy_seq)) (double calculate_entropy_total2))) (set! calculate_entropy_h2 (+ calculate_entropy_h2 (* calculate_entropy_prob (log2 calculate_entropy_prob)))))) (set! calculate_entropy_a1 (+ calculate_entropy_a1 1)))) (set! calculate_entropy_a0 (+ calculate_entropy_a0 1)))) (set! calculate_entropy_second_entropy (- calculate_entropy_h2)) (println (str (str (round_to_int calculate_entropy_second_entropy)) ".0")) (set! calculate_entropy_diff (- calculate_entropy_second_entropy calculate_entropy_first_entropy)) (println (str (str (round_to_int calculate_entropy_diff)) ".0")) calculate_entropy_text)))

(def ^:dynamic main_text1 (str (str "Behind Winston's back the voice " "from the telescreen was still ") "babbling and the overfulfilment"))

(def ^:dynamic main_text3 (str (str (str (str (str (str (str (str (str (str "Had repulsive dashwoods suspicion sincerity but advantage now him. " "Remark easily garret nor nay.  Civil those mrs enjoy shy fat merry. ") "You greatest jointure saw horrible. He private he on be imagine ") "suppose. Fertile beloved evident through no service elderly is. Blind ") "there if every no so at. Own neglected you preferred way sincerity ") "delivered his attempted. To of message cottage windows do besides ") "against uncivil.  Delightful unreserved impossible few estimating ") "men favourable see entreaties. She propriety immediate was improving. ") "He or entrance humoured likewise moderate. Much nor game son say ") "feel. Fat make met can must form into gate. Me we offending prevailed ") "discovery."))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (calculate_entropy main_text1)
      (calculate_entropy main_text3)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
