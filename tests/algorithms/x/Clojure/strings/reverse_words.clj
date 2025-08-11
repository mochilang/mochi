(ns main (:refer-clojure :exclude [split_words reverse_words main]))

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

(declare split_words reverse_words main)

(def ^:dynamic reverse_words_i nil)

(def ^:dynamic reverse_words_res nil)

(def ^:dynamic reverse_words_words nil)

(def ^:dynamic split_words_ch nil)

(def ^:dynamic split_words_current nil)

(def ^:dynamic split_words_i nil)

(def ^:dynamic split_words_words nil)

(defn split_words [split_words_s]
  (binding [split_words_ch nil split_words_current nil split_words_i nil split_words_words nil] (try (do (set! split_words_words []) (set! split_words_current "") (set! split_words_i 0) (while (< split_words_i (count split_words_s)) (do (set! split_words_ch (subs split_words_s split_words_i (+ split_words_i 1))) (if (= split_words_ch " ") (when (> (count split_words_current) 0) (do (set! split_words_words (conj split_words_words split_words_current)) (set! split_words_current ""))) (set! split_words_current (str split_words_current split_words_ch))) (set! split_words_i (+ split_words_i 1)))) (when (> (count split_words_current) 0) (set! split_words_words (conj split_words_words split_words_current))) (throw (ex-info "return" {:v split_words_words}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn reverse_words [reverse_words_input_str]
  (binding [reverse_words_i nil reverse_words_res nil reverse_words_words nil] (try (do (set! reverse_words_words (split_words reverse_words_input_str)) (set! reverse_words_res "") (set! reverse_words_i (- (count reverse_words_words) 1)) (while (>= reverse_words_i 0) (do (set! reverse_words_res (str reverse_words_res (nth reverse_words_words reverse_words_i))) (when (> reverse_words_i 0) (set! reverse_words_res (str reverse_words_res " "))) (set! reverse_words_i (- reverse_words_i 1)))) (throw (ex-info "return" {:v reverse_words_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (reverse_words "I love Python")) (println (reverse_words "I     Love          Python"))))

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
