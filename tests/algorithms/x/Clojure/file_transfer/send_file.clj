(ns main (:refer-clojure :exclude [send_file]))

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

(declare send_file)

(def ^:dynamic send_file_chunk nil)

(def ^:dynamic send_file_end nil)

(def ^:dynamic send_file_n nil)

(def ^:dynamic send_file_start nil)

(defn send_file [send_file_content send_file_chunk_size]
  (binding [send_file_chunk nil send_file_end nil send_file_n nil send_file_start nil] (do (set! send_file_start 0) (set! send_file_n (count send_file_content)) (while (< send_file_start send_file_n) (do (set! send_file_end (+ send_file_start send_file_chunk_size)) (when (> send_file_end send_file_n) (set! send_file_end send_file_n)) (set! send_file_chunk (subs send_file_content send_file_start (min send_file_end (count send_file_content)))) (println send_file_chunk) (set! send_file_start send_file_end))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (send_file "The quick brown fox jumps over the lazy dog." 10)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
