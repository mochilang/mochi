(ns main (:refer-clojure :exclude [triangular_numbers parse_words word_value contains solution]))

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

(declare triangular_numbers parse_words word_value contains solution)

(declare _read_file)

(def ^:dynamic contains_x nil)

(def ^:dynamic count_v nil)

(def ^:dynamic parse_words_c nil)

(def ^:dynamic parse_words_current nil)

(def ^:dynamic parse_words_i nil)

(def ^:dynamic parse_words_words nil)

(def ^:dynamic solution_text nil)

(def ^:dynamic solution_tri nil)

(def ^:dynamic solution_v nil)

(def ^:dynamic solution_w nil)

(def ^:dynamic solution_words nil)

(def ^:dynamic triangular_numbers_n nil)

(def ^:dynamic triangular_numbers_res nil)

(def ^:dynamic word_value_i nil)

(def ^:dynamic word_value_total nil)

(defn triangular_numbers [triangular_numbers_limit]
  (binding [triangular_numbers_n nil triangular_numbers_res nil] (try (do (set! triangular_numbers_res []) (set! triangular_numbers_n 1) (while (<= triangular_numbers_n triangular_numbers_limit) (do (set! triangular_numbers_res (conj triangular_numbers_res (/ (* triangular_numbers_n (+ triangular_numbers_n 1)) 2))) (set! triangular_numbers_n (+ triangular_numbers_n 1)))) (throw (ex-info "return" {:v triangular_numbers_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn parse_words [parse_words_text]
  (binding [parse_words_c nil parse_words_current nil parse_words_i nil parse_words_words nil] (try (do (set! parse_words_words []) (set! parse_words_current "") (set! parse_words_i 0) (while (< parse_words_i (count parse_words_text)) (do (set! parse_words_c (subs parse_words_text parse_words_i (min (+ parse_words_i 1) (count parse_words_text)))) (if (= parse_words_c ",") (do (set! parse_words_words (conj parse_words_words parse_words_current)) (set! parse_words_current "")) (if (= parse_words_c "\"") nil (if (or (= parse_words_c "\r") (= parse_words_c "\n")) nil (set! parse_words_current (str parse_words_current parse_words_c))))) (set! parse_words_i (+ parse_words_i 1)))) (when (> (count parse_words_current) 0) (set! parse_words_words (conj parse_words_words parse_words_current))) (throw (ex-info "return" {:v parse_words_words}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn word_value [word_value_word]
  (binding [word_value_i nil word_value_total nil] (try (do (set! word_value_total 0) (set! word_value_i 0) (while (< word_value_i (count word_value_word)) (do (set! word_value_total (- (+ word_value_total (_ord (subs word_value_word word_value_i (min (+ word_value_i 1) (count word_value_word))))) 64)) (set! word_value_i (+ word_value_i 1)))) (throw (ex-info "return" {:v word_value_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn contains [contains_xs contains_target]
  (binding [contains_x nil] (try (do (doseq [contains_x contains_xs] (when (= contains_x contains_target) (throw (ex-info "return" {:v true})))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution []
  (binding [count_v nil solution_text nil solution_tri nil solution_v nil solution_w nil solution_words nil] (try (do (set! solution_text (_read_file "words.txt")) (set! solution_words (parse_words solution_text)) (set! solution_tri (triangular_numbers 100)) (set! count_v 0) (doseq [solution_w solution_words] (do (set! solution_v (word_value solution_w)) (when (contains solution_tri solution_v) (set! count_v (+ count_v 1))))) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def _dataDir "/workspace/mochi/tests/github/TheAlgorithms/Mochi/project_euler/problem_042")

(defn _read_file [path]
  (let [f (java.io.File. path)] (try (slurp (if (.exists f) f (java.io.File. _dataDir path))) (catch Exception _ ""))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (mochi_str (solution)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
