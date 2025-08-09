(ns main (:refer-clojure :exclude [to_lowercase is_punct clean_text split contains floor round3 ln log10 term_frequency document_frequency inverse_document_frequency tf_idf]))

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

(declare to_lowercase is_punct clean_text split contains floor round3 ln log10 term_frequency document_frequency inverse_document_frequency tf_idf)

(def ^:dynamic clean_text_ch nil)

(def ^:dynamic clean_text_i nil)

(def ^:dynamic clean_text_lower nil)

(def ^:dynamic clean_text_res nil)

(def ^:dynamic contains_i nil)

(def ^:dynamic contains_is_match nil)

(def ^:dynamic contains_j nil)

(def ^:dynamic contains_m nil)

(def ^:dynamic contains_n nil)

(def ^:dynamic count_v nil)

(def ^:dynamic document_frequency_clean nil)

(def ^:dynamic document_frequency_docs nil)

(def ^:dynamic document_frequency_i nil)

(def ^:dynamic document_frequency_matches nil)

(def ^:dynamic document_frequency_t nil)

(def ^:dynamic floor_i nil)

(def ^:dynamic inverse_document_frequency_l nil)

(def ^:dynamic inverse_document_frequency_ratio nil)

(def ^:dynamic inverse_document_frequency_result nil)

(def ^:dynamic is_punct_i nil)

(def ^:dynamic ln_k nil)

(def ^:dynamic ln_sum nil)

(def ^:dynamic ln_t nil)

(def ^:dynamic ln_term nil)

(def ^:dynamic split_ch nil)

(def ^:dynamic split_current nil)

(def ^:dynamic split_i nil)

(def ^:dynamic split_res nil)

(def ^:dynamic term_frequency_clean nil)

(def ^:dynamic term_frequency_i nil)

(def ^:dynamic term_frequency_t nil)

(def ^:dynamic term_frequency_tokens nil)

(def ^:dynamic tf_idf_prod nil)

(def ^:dynamic tf_idf_result nil)

(def ^:dynamic to_lowercase_c nil)

(def ^:dynamic to_lowercase_found nil)

(def ^:dynamic to_lowercase_i nil)

(def ^:dynamic to_lowercase_j nil)

(def ^:dynamic to_lowercase_res nil)

(def ^:dynamic main_LOWER "abcdefghijklmnopqrstuvwxyz")

(def ^:dynamic main_UPPER "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(def ^:dynamic main_PUNCT "!\"#$%&'()*+,-./:;<=>?@[\\]^_{|}~")

(defn to_lowercase [to_lowercase_s]
  (binding [to_lowercase_c nil to_lowercase_found nil to_lowercase_i nil to_lowercase_j nil to_lowercase_res nil] (try (do (set! to_lowercase_res "") (set! to_lowercase_i 0) (while (< to_lowercase_i (count to_lowercase_s)) (do (set! to_lowercase_c (nth to_lowercase_s to_lowercase_i)) (set! to_lowercase_j 0) (set! to_lowercase_found false) (loop [while_flag_1 true] (when (and while_flag_1 (< to_lowercase_j (count main_UPPER))) (cond (= to_lowercase_c (nth main_UPPER to_lowercase_j)) (do (set! to_lowercase_res (str to_lowercase_res (nth main_LOWER to_lowercase_j))) (set! to_lowercase_found true) (recur false)) :else (do (set! to_lowercase_j (+ to_lowercase_j 1)) (recur while_flag_1))))) (when (not to_lowercase_found) (set! to_lowercase_res (str to_lowercase_res to_lowercase_c))) (set! to_lowercase_i (+ to_lowercase_i 1)))) (throw (ex-info "return" {:v to_lowercase_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_punct [is_punct_c]
  (binding [is_punct_i nil] (try (do (set! is_punct_i 0) (while (< is_punct_i (count main_PUNCT)) (do (when (= is_punct_c (nth main_PUNCT is_punct_i)) (throw (ex-info "return" {:v true}))) (set! is_punct_i (+ is_punct_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn clean_text [clean_text_text clean_text_keep_newlines]
  (binding [clean_text_ch nil clean_text_i nil clean_text_lower nil clean_text_res nil] (try (do (set! clean_text_lower (to_lowercase clean_text_text)) (set! clean_text_res "") (set! clean_text_i 0) (while (< clean_text_i (count clean_text_lower)) (do (set! clean_text_ch (nth clean_text_lower clean_text_i)) (if (is_punct clean_text_ch) nil (if (= clean_text_ch "\n") (when clean_text_keep_newlines (set! clean_text_res (str clean_text_res "\n"))) (set! clean_text_res (str clean_text_res clean_text_ch)))) (set! clean_text_i (+ clean_text_i 1)))) (throw (ex-info "return" {:v clean_text_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn split [split_s split_sep]
  (binding [split_ch nil split_current nil split_i nil split_res nil] (try (do (set! split_res []) (set! split_current "") (set! split_i 0) (while (< split_i (count split_s)) (do (set! split_ch (nth split_s split_i)) (if (= split_ch split_sep) (do (set! split_res (conj split_res split_current)) (set! split_current "")) (set! split_current (str split_current split_ch))) (set! split_i (+ split_i 1)))) (set! split_res (conj split_res split_current)) (throw (ex-info "return" {:v split_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn contains [contains_s contains_sub]
  (binding [contains_i nil contains_is_match nil contains_j nil contains_m nil contains_n nil] (try (do (set! contains_n (count contains_s)) (set! contains_m (count contains_sub)) (when (= contains_m 0) (throw (ex-info "return" {:v true}))) (set! contains_i 0) (while (<= contains_i (- contains_n contains_m)) (do (set! contains_j 0) (set! contains_is_match true) (loop [while_flag_2 true] (when (and while_flag_2 (< contains_j contains_m)) (cond (not= (nth contains_s (+ contains_i contains_j)) (nth contains_sub contains_j)) (do (set! contains_is_match false) (recur false)) :else (do (set! contains_j (+ contains_j 1)) (recur while_flag_2))))) (when contains_is_match (throw (ex-info "return" {:v true}))) (set! contains_i (+ contains_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn floor [floor_x]
  (binding [floor_i nil] (try (do (set! floor_i (long floor_x)) (when (> (double floor_i) floor_x) (set! floor_i (- floor_i 1))) (throw (ex-info "return" {:v (double floor_i)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn round3 [round3_x]
  (try (throw (ex-info "return" {:v (/ (floor (+ (* round3_x 1000.0) 0.5)) 1000.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn ln [ln_x]
  (binding [ln_k nil ln_sum nil ln_t nil ln_term nil] (try (do (set! ln_t (quot (- ln_x 1.0) (+ ln_x 1.0))) (set! ln_term ln_t) (set! ln_sum 0.0) (set! ln_k 1) (while (<= ln_k 99) (do (set! ln_sum (+ ln_sum (quot ln_term (double ln_k)))) (set! ln_term (* (* ln_term ln_t) ln_t)) (set! ln_k (+ ln_k 2)))) (throw (ex-info "return" {:v (* 2.0 ln_sum)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn log10 [log10_x]
  (try (throw (ex-info "return" {:v (quot (ln log10_x) (ln 10.0))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn term_frequency [term_frequency_term term_frequency_document]
  (binding [count_v nil term_frequency_clean nil term_frequency_i nil term_frequency_t nil term_frequency_tokens nil] (try (do (set! term_frequency_clean (clean_text term_frequency_document false)) (set! term_frequency_tokens (split term_frequency_clean " ")) (set! term_frequency_t (to_lowercase term_frequency_term)) (set! count_v 0) (set! term_frequency_i 0) (while (< term_frequency_i (count term_frequency_tokens)) (do (when (and (not= (nth term_frequency_tokens term_frequency_i) "") (= (nth term_frequency_tokens term_frequency_i) term_frequency_t)) (set! count_v (+ count_v 1))) (set! term_frequency_i (+ term_frequency_i 1)))) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn document_frequency [document_frequency_term document_frequency_corpus]
  (binding [document_frequency_clean nil document_frequency_docs nil document_frequency_i nil document_frequency_matches nil document_frequency_t nil] (try (do (set! document_frequency_clean (clean_text document_frequency_corpus true)) (set! document_frequency_docs (split document_frequency_clean "\n")) (set! document_frequency_t (to_lowercase document_frequency_term)) (set! document_frequency_matches 0) (set! document_frequency_i 0) (while (< document_frequency_i (count document_frequency_docs)) (do (when (contains (nth document_frequency_docs document_frequency_i) document_frequency_t) (set! document_frequency_matches (+ document_frequency_matches 1))) (set! document_frequency_i (+ document_frequency_i 1)))) (throw (ex-info "return" {:v [document_frequency_matches (count document_frequency_docs)]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn inverse_document_frequency [inverse_document_frequency_df inverse_document_frequency_n inverse_document_frequency_smoothing]
  (binding [inverse_document_frequency_l nil inverse_document_frequency_ratio nil inverse_document_frequency_result nil] (try (do (when inverse_document_frequency_smoothing (do (when (= inverse_document_frequency_n 0) (throw (Exception. "log10(0) is undefined."))) (set! inverse_document_frequency_ratio (quot (double inverse_document_frequency_n) (+ 1.0 (double inverse_document_frequency_df)))) (set! inverse_document_frequency_l (log10 inverse_document_frequency_ratio)) (set! inverse_document_frequency_result (round3 (+ 1.0 inverse_document_frequency_l))) (println inverse_document_frequency_result) (throw (ex-info "return" {:v inverse_document_frequency_result})))) (when (= inverse_document_frequency_df 0) (throw (Exception. "df must be > 0"))) (when (= inverse_document_frequency_n 0) (throw (Exception. "log10(0) is undefined."))) (set! inverse_document_frequency_ratio (quot (double inverse_document_frequency_n) (double inverse_document_frequency_df))) (set! inverse_document_frequency_l (log10 inverse_document_frequency_ratio)) (set! inverse_document_frequency_result (round3 inverse_document_frequency_l)) (println inverse_document_frequency_result) (throw (ex-info "return" {:v inverse_document_frequency_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn tf_idf [tf_idf_tf tf_idf_idf]
  (binding [tf_idf_prod nil tf_idf_result nil] (try (do (set! tf_idf_prod (* (double tf_idf_tf) tf_idf_idf)) (set! tf_idf_result (round3 tf_idf_prod)) (println tf_idf_result) (throw (ex-info "return" {:v tf_idf_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_corpus "This is the first document in the corpus.\nThIs is the second document in the corpus.\nTHIS is the third document in the corpus.")

(def ^:dynamic main_idf_val (inverse_document_frequency 1 3 false))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (term_frequency "to" "To be, or not to be"))
      (println (str (document_frequency "first" main_corpus)))
      (tf_idf 2 main_idf_val)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
