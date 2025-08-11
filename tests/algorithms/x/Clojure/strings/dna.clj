(ns main (:refer-clojure :exclude [is_valid dna]))

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

(declare is_valid dna)

(def ^:dynamic dna_ch nil)

(def ^:dynamic dna_i nil)

(def ^:dynamic dna_result nil)

(def ^:dynamic is_valid_ch nil)

(def ^:dynamic is_valid_i nil)

(defn is_valid [is_valid_strand]
  (binding [is_valid_ch nil is_valid_i nil] (try (do (set! is_valid_i 0) (while (< is_valid_i (count is_valid_strand)) (do (set! is_valid_ch (subs is_valid_strand is_valid_i (min (+ is_valid_i 1) (count is_valid_strand)))) (when (and (and (and (not= is_valid_ch "A") (not= is_valid_ch "T")) (not= is_valid_ch "C")) (not= is_valid_ch "G")) (throw (ex-info "return" {:v false}))) (set! is_valid_i (+ is_valid_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dna [dna_strand]
  (binding [dna_ch nil dna_i nil dna_result nil] (try (do (when (not (is_valid dna_strand)) (do (println "ValueError: Invalid Strand") (throw (ex-info "return" {:v ""})))) (set! dna_result "") (set! dna_i 0) (while (< dna_i (count dna_strand)) (do (set! dna_ch (subs dna_strand dna_i (min (+ dna_i 1) (count dna_strand)))) (if (= dna_ch "A") (set! dna_result (str dna_result "T")) (if (= dna_ch "T") (set! dna_result (str dna_result "A")) (if (= dna_ch "C") (set! dna_result (str dna_result "G")) (set! dna_result (str dna_result "C"))))) (set! dna_i (+ dna_i 1)))) (throw (ex-info "return" {:v dna_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (dna "GCTA"))
      (println (dna "ATGC"))
      (println (dna "CTGA"))
      (println (dna "GFGG"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
