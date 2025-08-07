(ns main (:refer-clojure :exclude [get_reverse_bit_string reverse_bit]))

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

(declare get_reverse_bit_string reverse_bit)

(def ^:dynamic get_reverse_bit_string_bit_string nil)

(def ^:dynamic get_reverse_bit_string_i nil)

(def ^:dynamic get_reverse_bit_string_n nil)

(def ^:dynamic reverse_bit_end_bit nil)

(def ^:dynamic reverse_bit_i nil)

(def ^:dynamic reverse_bit_n nil)

(def ^:dynamic reverse_bit_result nil)

(defn get_reverse_bit_string [get_reverse_bit_string_number]
  (binding [get_reverse_bit_string_bit_string nil get_reverse_bit_string_i nil get_reverse_bit_string_n nil] (try (do (set! get_reverse_bit_string_bit_string "") (set! get_reverse_bit_string_n get_reverse_bit_string_number) (set! get_reverse_bit_string_i 0) (while (< get_reverse_bit_string_i 32) (do (set! get_reverse_bit_string_bit_string (str get_reverse_bit_string_bit_string (str (mod get_reverse_bit_string_n 2)))) (set! get_reverse_bit_string_n (quot get_reverse_bit_string_n 2)) (set! get_reverse_bit_string_i (+ get_reverse_bit_string_i 1)))) (throw (ex-info "return" {:v get_reverse_bit_string_bit_string}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn reverse_bit [reverse_bit_number]
  (binding [reverse_bit_end_bit nil reverse_bit_i nil reverse_bit_n nil reverse_bit_result nil] (try (do (when (< reverse_bit_number 0) (throw (Exception. "the value of input must be positive"))) (set! reverse_bit_n reverse_bit_number) (set! reverse_bit_result 0) (set! reverse_bit_i 1) (while (<= reverse_bit_i 32) (do (set! reverse_bit_result (* reverse_bit_result 2)) (set! reverse_bit_end_bit (mod reverse_bit_n 2)) (set! reverse_bit_n (quot reverse_bit_n 2)) (set! reverse_bit_result (+ reverse_bit_result reverse_bit_end_bit)) (set! reverse_bit_i (+ reverse_bit_i 1)))) (throw (ex-info "return" {:v (get_reverse_bit_string reverse_bit_result)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (reverse_bit 25))
      (println (reverse_bit 37))
      (println (reverse_bit 21))
      (println (reverse_bit 58))
      (println (reverse_bit 0))
      (println (reverse_bit 256))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
