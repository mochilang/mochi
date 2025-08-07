(ns main (:refer-clojure :exclude [decimal_to_any main]))

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

(declare decimal_to_any main)

(def ^:dynamic decimal_to_any_digit nil)

(def ^:dynamic decimal_to_any_mod nil)

(def ^:dynamic decimal_to_any_n nil)

(def ^:dynamic decimal_to_any_result nil)

(def ^:dynamic decimal_to_any_symbols nil)

(defn decimal_to_any [decimal_to_any_num decimal_to_any_base]
  (binding [decimal_to_any_digit nil decimal_to_any_mod nil decimal_to_any_n nil decimal_to_any_result nil decimal_to_any_symbols nil] (try (do (when (< decimal_to_any_num 0) (throw (Exception. "parameter must be positive int"))) (when (< decimal_to_any_base 2) (throw (Exception. "base must be >= 2"))) (when (> decimal_to_any_base 36) (throw (Exception. "base must be <= 36"))) (when (= decimal_to_any_num 0) (throw (ex-info "return" {:v "0"}))) (set! decimal_to_any_symbols "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! decimal_to_any_n decimal_to_any_num) (set! decimal_to_any_result "") (while (> decimal_to_any_n 0) (do (set! decimal_to_any_mod (mod decimal_to_any_n decimal_to_any_base)) (set! decimal_to_any_digit (subs decimal_to_any_symbols decimal_to_any_mod (min (+ decimal_to_any_mod 1) (count decimal_to_any_symbols)))) (set! decimal_to_any_result (str decimal_to_any_digit decimal_to_any_result)) (set! decimal_to_any_n (/ decimal_to_any_n decimal_to_any_base)))) (throw (ex-info "return" {:v decimal_to_any_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (decimal_to_any 0 2)) (println (decimal_to_any 5 4)) (println (decimal_to_any 20 3)) (println (decimal_to_any 58 16)) (println (decimal_to_any 243 17))))

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
