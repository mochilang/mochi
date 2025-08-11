(ns main (:refer-clojure :exclude [split capitalize snake_to_camel_case]))

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

(declare split capitalize snake_to_camel_case)

(def ^:dynamic first_v nil)

(def ^:dynamic rest_v nil)

(def ^:dynamic snake_to_camel_case_index nil)

(def ^:dynamic snake_to_camel_case_result nil)

(def ^:dynamic snake_to_camel_case_word nil)

(def ^:dynamic snake_to_camel_case_words nil)

(def ^:dynamic split_ch nil)

(def ^:dynamic split_current nil)

(def ^:dynamic split_i nil)

(def ^:dynamic split_res nil)

(defn split [split_s split_sep]
  (binding [split_ch nil split_current nil split_i nil split_res nil] (try (do (set! split_res []) (set! split_current "") (set! split_i 0) (while (< split_i (count split_s)) (do (set! split_ch (subs split_s split_i (min (+ split_i 1) (count split_s)))) (if (= split_ch split_sep) (do (set! split_res (conj split_res split_current)) (set! split_current "")) (set! split_current (str split_current split_ch))) (set! split_i (+ split_i 1)))) (set! split_res (conj split_res split_current)) (throw (ex-info "return" {:v split_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn capitalize [capitalize_word]
  (binding [first_v nil rest_v nil] (try (do (when (= (count capitalize_word) 0) (throw (ex-info "return" {:v ""}))) (set! first_v (clojure.string/upper-case (subs capitalize_word 0 (min 1 (count capitalize_word))))) (set! rest_v (subs capitalize_word 1 (min (count capitalize_word) (count capitalize_word)))) (throw (ex-info "return" {:v (str first_v rest_v)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn snake_to_camel_case [snake_to_camel_case_input_str snake_to_camel_case_use_pascal]
  (binding [snake_to_camel_case_index nil snake_to_camel_case_result nil snake_to_camel_case_word nil snake_to_camel_case_words nil] (try (do (set! snake_to_camel_case_words (split snake_to_camel_case_input_str "_")) (set! snake_to_camel_case_result "") (set! snake_to_camel_case_index 0) (when (not snake_to_camel_case_use_pascal) (when (> (count snake_to_camel_case_words) 0) (do (set! snake_to_camel_case_result (nth snake_to_camel_case_words 0)) (set! snake_to_camel_case_index 1)))) (while (< snake_to_camel_case_index (count snake_to_camel_case_words)) (do (set! snake_to_camel_case_word (nth snake_to_camel_case_words snake_to_camel_case_index)) (set! snake_to_camel_case_result (str snake_to_camel_case_result (capitalize snake_to_camel_case_word))) (set! snake_to_camel_case_index (+ snake_to_camel_case_index 1)))) (throw (ex-info "return" {:v snake_to_camel_case_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (snake_to_camel_case "some_random_string" false))
      (println (snake_to_camel_case "some_random_string" true))
      (println (snake_to_camel_case "some_random_string_with_numbers_123" false))
      (println (snake_to_camel_case "some_random_string_with_numbers_123" true))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
