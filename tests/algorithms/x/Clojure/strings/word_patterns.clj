(ns main (:refer-clojure :exclude [find_index get_word_pattern main]))

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

(declare find_index get_word_pattern main)

(def ^:dynamic find_index_i nil)

(def ^:dynamic get_word_pattern_ch nil)

(def ^:dynamic get_word_pattern_i nil)

(def ^:dynamic get_word_pattern_idx nil)

(def ^:dynamic get_word_pattern_letters nil)

(def ^:dynamic get_word_pattern_next_num nil)

(def ^:dynamic get_word_pattern_num_str nil)

(def ^:dynamic get_word_pattern_numbers nil)

(def ^:dynamic get_word_pattern_res nil)

(def ^:dynamic get_word_pattern_w nil)

(defn find_index [find_index_xs find_index_x]
  (binding [find_index_i nil] (try (do (set! find_index_i 0) (while (< find_index_i (count find_index_xs)) (do (when (= (nth find_index_xs find_index_i) find_index_x) (throw (ex-info "return" {:v find_index_i}))) (set! find_index_i (+ find_index_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_word_pattern [get_word_pattern_word]
  (binding [get_word_pattern_ch nil get_word_pattern_i nil get_word_pattern_idx nil get_word_pattern_letters nil get_word_pattern_next_num nil get_word_pattern_num_str nil get_word_pattern_numbers nil get_word_pattern_res nil get_word_pattern_w nil] (try (do (set! get_word_pattern_w (clojure.string/upper-case get_word_pattern_word)) (set! get_word_pattern_letters []) (set! get_word_pattern_numbers []) (set! get_word_pattern_next_num 0) (set! get_word_pattern_res "") (set! get_word_pattern_i 0) (while (< get_word_pattern_i (count get_word_pattern_w)) (do (set! get_word_pattern_ch (nth get_word_pattern_w get_word_pattern_i)) (set! get_word_pattern_idx (find_index get_word_pattern_letters get_word_pattern_ch)) (set! get_word_pattern_num_str "") (if (>= get_word_pattern_idx 0) (set! get_word_pattern_num_str (nth get_word_pattern_numbers get_word_pattern_idx)) (do (set! get_word_pattern_num_str (str get_word_pattern_next_num)) (set! get_word_pattern_letters (conj get_word_pattern_letters get_word_pattern_ch)) (set! get_word_pattern_numbers (conj get_word_pattern_numbers get_word_pattern_num_str)) (set! get_word_pattern_next_num (+ get_word_pattern_next_num 1)))) (when (> get_word_pattern_i 0) (set! get_word_pattern_res (str get_word_pattern_res "."))) (set! get_word_pattern_res (str get_word_pattern_res get_word_pattern_num_str)) (set! get_word_pattern_i (+ get_word_pattern_i 1)))) (throw (ex-info "return" {:v get_word_pattern_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (get_word_pattern "")) (println (get_word_pattern " ")) (println (get_word_pattern "pattern")) (println (get_word_pattern "word pattern")) (println (get_word_pattern "get word pattern"))))

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
