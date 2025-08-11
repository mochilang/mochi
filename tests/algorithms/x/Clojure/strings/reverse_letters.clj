(ns main (:refer-clojure :exclude [split join_with_space reverse_str reverse_letters test_reverse_letters main]))

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

(declare split join_with_space reverse_str reverse_letters test_reverse_letters main)

(def ^:dynamic join_with_space_i nil)

(def ^:dynamic join_with_space_s nil)

(def ^:dynamic reverse_letters_i nil)

(def ^:dynamic reverse_letters_result nil)

(def ^:dynamic reverse_letters_word nil)

(def ^:dynamic reverse_letters_words nil)

(def ^:dynamic reverse_str_i nil)

(def ^:dynamic reverse_str_res nil)

(def ^:dynamic split_ch nil)

(def ^:dynamic split_current nil)

(def ^:dynamic split_i nil)

(def ^:dynamic split_res nil)

(defn split [split_s split_sep]
  (binding [split_ch nil split_current nil split_i nil split_res nil] (try (do (set! split_res []) (set! split_current "") (set! split_i 0) (while (< split_i (count split_s)) (do (set! split_ch (subs split_s split_i (+ split_i 1))) (if (= split_ch split_sep) (do (set! split_res (conj split_res split_current)) (set! split_current "")) (set! split_current (str split_current split_ch))) (set! split_i (+ split_i 1)))) (set! split_res (conj split_res split_current)) (throw (ex-info "return" {:v split_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn join_with_space [join_with_space_xs]
  (binding [join_with_space_i nil join_with_space_s nil] (try (do (set! join_with_space_s "") (set! join_with_space_i 0) (while (< join_with_space_i (count join_with_space_xs)) (do (set! join_with_space_s (str join_with_space_s (nth join_with_space_xs join_with_space_i))) (when (< (+ join_with_space_i 1) (count join_with_space_xs)) (set! join_with_space_s (str join_with_space_s " "))) (set! join_with_space_i (+ join_with_space_i 1)))) (throw (ex-info "return" {:v join_with_space_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn reverse_str [reverse_str_s]
  (binding [reverse_str_i nil reverse_str_res nil] (try (do (set! reverse_str_res "") (set! reverse_str_i (- (count reverse_str_s) 1)) (while (>= reverse_str_i 0) (do (set! reverse_str_res (str reverse_str_res (subs reverse_str_s reverse_str_i (+ reverse_str_i 1)))) (set! reverse_str_i (- reverse_str_i 1)))) (throw (ex-info "return" {:v reverse_str_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn reverse_letters [reverse_letters_sentence reverse_letters_length]
  (binding [reverse_letters_i nil reverse_letters_result nil reverse_letters_word nil reverse_letters_words nil] (try (do (set! reverse_letters_words (split reverse_letters_sentence " ")) (set! reverse_letters_result []) (set! reverse_letters_i 0) (while (< reverse_letters_i (count reverse_letters_words)) (do (set! reverse_letters_word (nth reverse_letters_words reverse_letters_i)) (if (> (count reverse_letters_word) reverse_letters_length) (set! reverse_letters_result (conj reverse_letters_result (reverse_str reverse_letters_word))) (set! reverse_letters_result (conj reverse_letters_result reverse_letters_word))) (set! reverse_letters_i (+ reverse_letters_i 1)))) (throw (ex-info "return" {:v (join_with_space reverse_letters_result)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_reverse_letters []
  (do (when (not= (reverse_letters "Hey wollef sroirraw" 3) "Hey fellow warriors") (throw (Exception. "test1 failed"))) (when (not= (reverse_letters "nohtyP is nohtyP" 2) "Python is Python") (throw (Exception. "test2 failed"))) (when (not= (reverse_letters "1 12 123 1234 54321 654321" 0) "1 21 321 4321 12345 123456") (throw (Exception. "test3 failed"))) (when (not= (reverse_letters "racecar" 0) "racecar") (throw (Exception. "test4 failed")))))

(defn main []
  (do (test_reverse_letters) (println (reverse_letters "Hey wollef sroirraw" 3))))

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
