(ns main (:refer-clojure :exclude [decimal_to_binary_iterative decimal_to_binary_recursive_helper decimal_to_binary_recursive]))

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

(declare decimal_to_binary_iterative decimal_to_binary_recursive_helper decimal_to_binary_recursive)

(def ^:dynamic decimal_to_binary_iterative_n nil)

(def ^:dynamic decimal_to_binary_iterative_negative nil)

(def ^:dynamic decimal_to_binary_iterative_result nil)

(def ^:dynamic decimal_to_binary_recursive_helper_div nil)

(def ^:dynamic decimal_to_binary_recursive_helper_mod nil)

(defn decimal_to_binary_iterative [decimal_to_binary_iterative_num]
  (binding [decimal_to_binary_iterative_n nil decimal_to_binary_iterative_negative nil decimal_to_binary_iterative_result nil] (try (do (when (= decimal_to_binary_iterative_num 0) (throw (ex-info "return" {:v "0b0"}))) (set! decimal_to_binary_iterative_negative false) (set! decimal_to_binary_iterative_n decimal_to_binary_iterative_num) (when (< decimal_to_binary_iterative_n 0) (do (set! decimal_to_binary_iterative_negative true) (set! decimal_to_binary_iterative_n (- decimal_to_binary_iterative_n)))) (set! decimal_to_binary_iterative_result "") (while (> decimal_to_binary_iterative_n 0) (do (set! decimal_to_binary_iterative_result (str (str (mod decimal_to_binary_iterative_n 2)) decimal_to_binary_iterative_result)) (set! decimal_to_binary_iterative_n (quot decimal_to_binary_iterative_n 2)))) (if decimal_to_binary_iterative_negative (str "-0b" decimal_to_binary_iterative_result) (str "0b" decimal_to_binary_iterative_result))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decimal_to_binary_recursive_helper [decimal_to_binary_recursive_helper_n]
  (binding [decimal_to_binary_recursive_helper_div nil decimal_to_binary_recursive_helper_mod nil] (try (do (when (= decimal_to_binary_recursive_helper_n 0) (throw (ex-info "return" {:v "0"}))) (when (= decimal_to_binary_recursive_helper_n 1) (throw (ex-info "return" {:v "1"}))) (set! decimal_to_binary_recursive_helper_div (quot decimal_to_binary_recursive_helper_n 2)) (set! decimal_to_binary_recursive_helper_mod (mod decimal_to_binary_recursive_helper_n 2)) (throw (ex-info "return" {:v (str (decimal_to_binary_recursive_helper decimal_to_binary_recursive_helper_div) (str decimal_to_binary_recursive_helper_mod))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decimal_to_binary_recursive [decimal_to_binary_recursive_num]
  (try (do (when (= decimal_to_binary_recursive_num 0) (throw (ex-info "return" {:v "0b0"}))) (if (< decimal_to_binary_recursive_num 0) (str "-0b" (decimal_to_binary_recursive_helper (- decimal_to_binary_recursive_num))) (str "0b" (decimal_to_binary_recursive_helper decimal_to_binary_recursive_num)))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (decimal_to_binary_iterative 0))
      (println (decimal_to_binary_iterative 2))
      (println (decimal_to_binary_iterative 7))
      (println (decimal_to_binary_iterative 35))
      (println (decimal_to_binary_iterative (- 2)))
      (println (decimal_to_binary_recursive 0))
      (println (decimal_to_binary_recursive 40))
      (println (decimal_to_binary_recursive (- 40)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
