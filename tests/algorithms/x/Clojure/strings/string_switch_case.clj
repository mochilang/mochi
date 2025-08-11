(ns main (:refer-clojure :exclude [split_words is_alnum split_input capitalize to_simple_case to_complex_case to_pascal_case to_camel_case to_snake_case to_kebab_case]))

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

(declare split_words is_alnum split_input capitalize to_simple_case to_complex_case to_pascal_case to_camel_case to_snake_case to_kebab_case)

(def ^:dynamic first_v nil)

(def ^:dynamic split_input_current nil)

(def ^:dynamic split_input_result nil)

(def ^:dynamic split_words_current nil)

(def ^:dynamic split_words_words nil)

(def ^:dynamic to_camel_case_s nil)

(def ^:dynamic to_complex_case_parts nil)

(def ^:dynamic to_complex_case_res nil)

(def ^:dynamic to_complex_case_word nil)

(def ^:dynamic to_simple_case_parts nil)

(def ^:dynamic to_simple_case_res nil)

(defn split_words [split_words_s]
  (binding [split_words_current nil split_words_words nil] (try (do (set! split_words_words []) (set! split_words_current "") (doseq [ch split_words_s] (if (= ch " ") (when (not= split_words_current "") (do (set! split_words_words (conj split_words_words split_words_current)) (set! split_words_current ""))) (set! split_words_current (str split_words_current ch)))) (when (not= split_words_current "") (set! split_words_words (conj split_words_words split_words_current))) (throw (ex-info "return" {:v split_words_words}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_alnum [is_alnum_c]
  (try (throw (ex-info "return" {:v (or (or (or (in is_alnum_c "0123456789") (in is_alnum_c "abcdefghijklmnopqrstuvwxyz")) (in is_alnum_c "ABCDEFGHIJKLMNOPQRSTUVWXYZ")) (= is_alnum_c " "))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn split_input [split_input_text]
  (binding [split_input_current nil split_input_result nil] (try (do (set! split_input_result []) (set! split_input_current "") (doseq [ch split_input_text] (if (is_alnum ch) (set! split_input_current (str split_input_current ch)) (when (not= split_input_current "") (do (set! split_input_result (conj split_input_result (split_words split_input_current))) (set! split_input_current ""))))) (when (not= split_input_current "") (set! split_input_result (conj split_input_result (split_words split_input_current)))) (throw (ex-info "return" {:v split_input_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn capitalize [capitalize_word]
  (try (do (when (= (count capitalize_word) 0) (throw (ex-info "return" {:v ""}))) (if (= (count capitalize_word) 1) (clojure.string/upper-case capitalize_word) (+ (clojure.string/upper-case (subs capitalize_word 0 (min 1 (count capitalize_word)))) (clojure.string/lower-case (subs capitalize_word 1 (min (count capitalize_word) (count capitalize_word))))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn to_simple_case [to_simple_case_text]
  (binding [to_simple_case_parts nil to_simple_case_res nil] (try (do (set! to_simple_case_parts (split_input to_simple_case_text)) (set! to_simple_case_res "") (doseq [sub to_simple_case_parts] (doseq [w sub] (set! to_simple_case_res (str to_simple_case_res (capitalize w))))) (throw (ex-info "return" {:v to_simple_case_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn to_complex_case [to_complex_case_text to_complex_case_upper_flag to_complex_case_sep]
  (binding [first_v nil to_complex_case_parts nil to_complex_case_res nil to_complex_case_word nil] (try (do (set! to_complex_case_parts (split_input to_complex_case_text)) (set! to_complex_case_res "") (doseq [sub to_complex_case_parts] (do (set! first_v true) (doseq [w sub] (do (set! to_complex_case_word (if to_complex_case_upper_flag (clojure.string/upper-case w) (clojure.string/lower-case w))) (if first_v (do (set! to_complex_case_res (str to_complex_case_res to_complex_case_word)) (set! first_v false)) (set! to_complex_case_res (str (str to_complex_case_res to_complex_case_sep) to_complex_case_word))))))) (throw (ex-info "return" {:v to_complex_case_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn to_pascal_case [to_pascal_case_text]
  (try (throw (ex-info "return" {:v (to_simple_case to_pascal_case_text)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn to_camel_case [to_camel_case_text]
  (binding [to_camel_case_s nil] (try (do (set! to_camel_case_s (to_simple_case to_camel_case_text)) (if (= (count to_camel_case_s) 0) "" (str (clojure.string/lower-case (subs to_camel_case_s 0 (min 1 (count to_camel_case_s)))) (subs to_camel_case_s 1 (min (count to_camel_case_s) (count to_camel_case_s)))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn to_snake_case [to_snake_case_text to_snake_case_upper_flag]
  (try (throw (ex-info "return" {:v (to_complex_case to_snake_case_text to_snake_case_upper_flag "_")})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn to_kebab_case [to_kebab_case_text to_kebab_case_upper_flag]
  (try (throw (ex-info "return" {:v (to_complex_case to_kebab_case_text to_kebab_case_upper_flag "-")})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (to_pascal_case "one two 31235three4four"))
      (println (to_camel_case "one two 31235three4four"))
      (println (to_snake_case "one two 31235three4four" true))
      (println (to_snake_case "one two 31235three4four" false))
      (println (to_kebab_case "one two 31235three4four" true))
      (println (to_kebab_case "one two 31235three4four" false))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
