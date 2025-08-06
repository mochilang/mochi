(ns main (:refer-clojure :exclude [get_value contains_value backtrack match_word_pattern main]))

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

(declare get_value contains_value backtrack match_word_pattern main)

(def ^:dynamic backtrack_ch nil)

(def ^:dynamic backtrack_end nil)

(def ^:dynamic backtrack_mapped nil)

(def ^:dynamic backtrack_new_keys nil)

(def ^:dynamic backtrack_new_values nil)

(def ^:dynamic backtrack_substr nil)

(def ^:dynamic contains_value_i nil)

(def ^:dynamic get_value_i nil)

(def ^:dynamic match_word_pattern_keys nil)

(def ^:dynamic match_word_pattern_values nil)

(defn get_value [get_value_keys get_value_values get_value_key]
  (binding [get_value_i nil] (try (do (set! get_value_i 0) (while (< get_value_i (count get_value_keys)) (do (when (= (nth get_value_keys get_value_i) get_value_key) (throw (ex-info "return" {:v (nth get_value_values get_value_i)}))) (set! get_value_i (+ get_value_i 1)))) (throw (ex-info "return" {:v nil}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn contains_value [contains_value_values contains_value_value]
  (binding [contains_value_i nil] (try (do (set! contains_value_i 0) (while (< contains_value_i (count contains_value_values)) (do (when (= (nth contains_value_values contains_value_i) contains_value_value) (throw (ex-info "return" {:v true}))) (set! contains_value_i (+ contains_value_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn backtrack [backtrack_pattern backtrack_input_string backtrack_pi backtrack_si backtrack_keys backtrack_values]
  (binding [backtrack_ch nil backtrack_end nil backtrack_mapped nil backtrack_new_keys nil backtrack_new_values nil backtrack_substr nil] (try (do (when (and (= backtrack_pi (count backtrack_pattern)) (= backtrack_si (count backtrack_input_string))) (throw (ex-info "return" {:v true}))) (when (or (= backtrack_pi (count backtrack_pattern)) (= backtrack_si (count backtrack_input_string))) (throw (ex-info "return" {:v false}))) (set! backtrack_ch (subs backtrack_pattern backtrack_pi (min (+ backtrack_pi 1) (count backtrack_pattern)))) (set! backtrack_mapped (get_value backtrack_keys backtrack_values backtrack_ch)) (when (not= backtrack_mapped nil) (do (when (= (subs backtrack_input_string backtrack_si (min (+ backtrack_si (count backtrack_mapped)) (count backtrack_input_string))) backtrack_mapped) (throw (ex-info "return" {:v (backtrack backtrack_pattern backtrack_input_string (+ backtrack_pi 1) (+ backtrack_si (count backtrack_mapped)) backtrack_keys backtrack_values)}))) (throw (ex-info "return" {:v false})))) (set! backtrack_end (+ backtrack_si 1)) (loop [while_flag_1 true] (when (and while_flag_1 (<= backtrack_end (count backtrack_input_string))) (do (set! backtrack_substr (subs backtrack_input_string backtrack_si (min backtrack_end (count backtrack_input_string)))) (cond (contains_value backtrack_values backtrack_substr) (do (set! backtrack_end (+ backtrack_end 1)) (recur true)) :else (do (set! backtrack_new_keys (conj backtrack_keys backtrack_ch)) (set! backtrack_new_values (conj backtrack_values backtrack_substr)) (when (backtrack backtrack_pattern backtrack_input_string (+ backtrack_pi 1) backtrack_end backtrack_new_keys backtrack_new_values) (throw (ex-info "return" {:v true}))) (set! backtrack_end (+ backtrack_end 1)) (recur while_flag_1)))))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn match_word_pattern [match_word_pattern_pattern match_word_pattern_input_string]
  (binding [match_word_pattern_keys nil match_word_pattern_values nil] (try (do (set! match_word_pattern_keys []) (set! match_word_pattern_values []) (throw (ex-info "return" {:v (backtrack match_word_pattern_pattern match_word_pattern_input_string 0 0 match_word_pattern_keys match_word_pattern_values)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (match_word_pattern "aba" "GraphTreesGraph")) (println (match_word_pattern "xyx" "PythonRubyPython")) (println (match_word_pattern "GG" "PythonJavaPython"))))

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
