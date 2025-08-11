(ns main (:refer-clojure :exclude [word_occurrence main]))

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

(declare word_occurrence main)

(def ^:dynamic main_result nil)

(def ^:dynamic word_occurrence_ch nil)

(def ^:dynamic word_occurrence_i nil)

(def ^:dynamic word_occurrence_occurrence nil)

(def ^:dynamic word_occurrence_word nil)

(defn word_occurrence [word_occurrence_sentence]
  (binding [word_occurrence_ch nil word_occurrence_i nil word_occurrence_occurrence nil word_occurrence_word nil] (try (do (set! word_occurrence_occurrence {}) (set! word_occurrence_word "") (set! word_occurrence_i 0) (while (< word_occurrence_i (count word_occurrence_sentence)) (do (set! word_occurrence_ch (subs word_occurrence_sentence word_occurrence_i (min (+ word_occurrence_i 1) (count word_occurrence_sentence)))) (if (= word_occurrence_ch " ") (when (not= word_occurrence_word "") (do (if (in word_occurrence_word word_occurrence_occurrence) (set! word_occurrence_occurrence (assoc word_occurrence_occurrence word_occurrence_word (+ (get word_occurrence_occurrence word_occurrence_word) 1))) (set! word_occurrence_occurrence (assoc word_occurrence_occurrence word_occurrence_word 1))) (set! word_occurrence_word ""))) (set! word_occurrence_word (str word_occurrence_word word_occurrence_ch))) (set! word_occurrence_i (+ word_occurrence_i 1)))) (when (not= word_occurrence_word "") (if (in word_occurrence_word word_occurrence_occurrence) (set! word_occurrence_occurrence (assoc word_occurrence_occurrence word_occurrence_word (+ (get word_occurrence_occurrence word_occurrence_word) 1))) (set! word_occurrence_occurrence (assoc word_occurrence_occurrence word_occurrence_word 1)))) (throw (ex-info "return" {:v word_occurrence_occurrence}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_result nil] (do (set! main_result (word_occurrence "INPUT STRING")) (doseq [w main_result] (println (str (str w ": ") (str (nth main_result w))))))))

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
