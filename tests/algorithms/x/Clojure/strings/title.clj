(ns main (:refer-clojure :exclude [index_of to_title_case split_words sentence_to_title_case]))

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

(declare index_of to_title_case split_words sentence_to_title_case)

(def ^:dynamic first_v nil)

(def ^:dynamic index_of_i nil)

(def ^:dynamic sentence_to_title_case_i nil)

(def ^:dynamic sentence_to_title_case_res nil)

(def ^:dynamic sentence_to_title_case_words nil)

(def ^:dynamic split_words_ch nil)

(def ^:dynamic split_words_current nil)

(def ^:dynamic split_words_i nil)

(def ^:dynamic split_words_words nil)

(def ^:dynamic to_title_case_ch nil)

(def ^:dynamic to_title_case_i nil)

(def ^:dynamic to_title_case_idx nil)

(def ^:dynamic to_title_case_result nil)

(def ^:dynamic to_title_case_uidx nil)

(def ^:dynamic main_lower "abcdefghijklmnopqrstuvwxyz")

(def ^:dynamic main_upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn index_of [index_of_s index_of_ch]
  (binding [index_of_i nil] (try (do (set! index_of_i 0) (while (< index_of_i (count index_of_s)) (do (when (= (subs index_of_s index_of_i (+ index_of_i 1)) index_of_ch) (throw (ex-info "return" {:v index_of_i}))) (set! index_of_i (+ index_of_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn to_title_case [to_title_case_word]
  (binding [first_v nil to_title_case_ch nil to_title_case_i nil to_title_case_idx nil to_title_case_result nil to_title_case_uidx nil] (try (do (when (= (count to_title_case_word) 0) (throw (ex-info "return" {:v ""}))) (set! first_v (subs to_title_case_word 0 (min 1 (count to_title_case_word)))) (set! to_title_case_idx (index_of main_lower first_v)) (set! to_title_case_result (if (>= to_title_case_idx 0) (subs main_upper to_title_case_idx (min (+ to_title_case_idx 1) (count main_upper))) first_v)) (set! to_title_case_i 1) (while (< to_title_case_i (count to_title_case_word)) (do (set! to_title_case_ch (subs to_title_case_word to_title_case_i (min (+ to_title_case_i 1) (count to_title_case_word)))) (set! to_title_case_uidx (index_of main_upper to_title_case_ch)) (if (>= to_title_case_uidx 0) (set! to_title_case_result (str to_title_case_result (subs main_lower to_title_case_uidx (min (+ to_title_case_uidx 1) (count main_lower))))) (set! to_title_case_result (str to_title_case_result to_title_case_ch))) (set! to_title_case_i (+ to_title_case_i 1)))) (throw (ex-info "return" {:v to_title_case_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn split_words [split_words_s]
  (binding [split_words_ch nil split_words_current nil split_words_i nil split_words_words nil] (try (do (set! split_words_words []) (set! split_words_current "") (set! split_words_i 0) (while (< split_words_i (count split_words_s)) (do (set! split_words_ch (subs split_words_s split_words_i (+ split_words_i 1))) (if (= split_words_ch " ") (when (> (count split_words_current) 0) (do (set! split_words_words (conj split_words_words split_words_current)) (set! split_words_current ""))) (set! split_words_current (str split_words_current split_words_ch))) (set! split_words_i (+ split_words_i 1)))) (when (> (count split_words_current) 0) (set! split_words_words (conj split_words_words split_words_current))) (throw (ex-info "return" {:v split_words_words}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sentence_to_title_case [sentence_to_title_case_sentence]
  (binding [sentence_to_title_case_i nil sentence_to_title_case_res nil sentence_to_title_case_words nil] (try (do (set! sentence_to_title_case_words (split_words sentence_to_title_case_sentence)) (set! sentence_to_title_case_res "") (set! sentence_to_title_case_i 0) (while (< sentence_to_title_case_i (count sentence_to_title_case_words)) (do (set! sentence_to_title_case_res (str sentence_to_title_case_res (to_title_case (nth sentence_to_title_case_words sentence_to_title_case_i)))) (when (< (+ sentence_to_title_case_i 1) (count sentence_to_title_case_words)) (set! sentence_to_title_case_res (str sentence_to_title_case_res " "))) (set! sentence_to_title_case_i (+ sentence_to_title_case_i 1)))) (throw (ex-info "return" {:v sentence_to_title_case_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (to_title_case "Aakash"))
      (println (to_title_case "aakash"))
      (println (to_title_case "AAKASH"))
      (println (to_title_case "aAkAsH"))
      (println (sentence_to_title_case "Aakash Giri"))
      (println (sentence_to_title_case "aakash giri"))
      (println (sentence_to_title_case "AAKASH GIRI"))
      (println (sentence_to_title_case "aAkAsH gIrI"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
