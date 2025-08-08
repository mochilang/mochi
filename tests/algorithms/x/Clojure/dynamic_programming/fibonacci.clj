(ns main (:refer-clojure :exclude [create_fibonacci fib_get main]))

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

(declare create_fibonacci fib_get main)

(def ^:dynamic fib_get_f nil)

(def ^:dynamic fib_get_i nil)

(def ^:dynamic fib_get_result nil)

(def ^:dynamic fib_get_seq nil)

(def ^:dynamic main_fib nil)

(def ^:dynamic main_res nil)

(def ^:dynamic next_v nil)

(defn create_fibonacci []
  (try (throw (ex-info "return" {:v {:sequence [0 1]}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fib_get [fib_get_f_p fib_get_index]
  (binding [fib_get_f nil fib_get_i nil fib_get_result nil fib_get_seq nil next_v nil] (try (do (set! fib_get_f fib_get_f_p) (set! fib_get_seq (:sequence fib_get_f)) (while (< (count fib_get_seq) fib_get_index) (do (set! next_v (+ (nth fib_get_seq (- (count fib_get_seq) 1)) (nth fib_get_seq (- (count fib_get_seq) 2)))) (set! fib_get_seq (conj fib_get_seq next_v)))) (set! fib_get_f (assoc fib_get_f :sequence fib_get_seq)) (set! fib_get_result []) (set! fib_get_i 0) (while (< fib_get_i fib_get_index) (do (set! fib_get_result (conj fib_get_result (nth fib_get_seq fib_get_i))) (set! fib_get_i (+ fib_get_i 1)))) (throw (ex-info "return" {:v {:fib fib_get_f :values fib_get_result}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_fib nil main_res nil] (do (set! main_fib (create_fibonacci)) (set! main_res (fib_get main_fib 10)) (set! main_fib (:fib main_res)) (println (str (:values main_res))) (set! main_res (fib_get main_fib 5)) (set! main_fib (:fib main_res)) (println (str (:values main_res))))))

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
