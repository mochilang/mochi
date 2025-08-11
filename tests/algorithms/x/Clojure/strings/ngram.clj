(ns main (:refer-clojure :exclude [create_ngram]))

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

(declare create_ngram)

(def ^:dynamic create_ngram_bound nil)

(def ^:dynamic create_ngram_i nil)

(def ^:dynamic create_ngram_res nil)

(defn create_ngram [create_ngram_sentence create_ngram_ngram_size]
  (binding [create_ngram_bound nil create_ngram_i nil create_ngram_res nil] (try (do (set! create_ngram_res []) (set! create_ngram_bound (+ (- (count create_ngram_sentence) create_ngram_ngram_size) 1)) (when (<= create_ngram_bound 0) (throw (ex-info "return" {:v create_ngram_res}))) (set! create_ngram_i 0) (while (< create_ngram_i create_ngram_bound) (do (set! create_ngram_res (conj create_ngram_res (subs create_ngram_sentence create_ngram_i (min (+ create_ngram_i create_ngram_ngram_size) (count create_ngram_sentence))))) (set! create_ngram_i (+ create_ngram_i 1)))) (throw (ex-info "return" {:v create_ngram_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_example1 "I am a sentence")

(def ^:dynamic main_example2 "I am an NLPer")

(def ^:dynamic main_example3 "This is short")

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (create_ngram main_example1 2))
      (println (create_ngram main_example2 2))
      (println (create_ngram main_example3 50))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
