(ns main (:refer-clojure :exclude [collatz_sequence main]))

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

(declare collatz_sequence main)

(def ^:dynamic collatz_sequence_current nil)

(def ^:dynamic collatz_sequence_seq nil)

(def ^:dynamic main_n nil)

(def ^:dynamic main_seq nil)

(defn collatz_sequence [collatz_sequence_n]
  (binding [collatz_sequence_current nil collatz_sequence_seq nil] (try (do (when (< collatz_sequence_n 1) (throw (Exception. "Sequence only defined for positive integers"))) (set! collatz_sequence_seq [collatz_sequence_n]) (set! collatz_sequence_current collatz_sequence_n) (while (not= collatz_sequence_current 1) (do (if (= (mod collatz_sequence_current 2) 0) (set! collatz_sequence_current (quot collatz_sequence_current 2)) (set! collatz_sequence_current (+ (* 3 collatz_sequence_current) 1))) (set! collatz_sequence_seq (conj collatz_sequence_seq collatz_sequence_current)))) (throw (ex-info "return" {:v collatz_sequence_seq}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_n nil main_seq nil] (do (set! main_n 11) (set! main_seq (collatz_sequence main_n)) (println (str main_seq)) (println (str (str (str (str "Collatz sequence from " (str main_n)) " took ") (str (count main_seq))) " steps.")))))

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
