(ns main (:refer-clojure :exclude [next_greatest_element_slow next_greatest_element_fast set_at_float next_greatest_element]))

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

(declare next_greatest_element_slow next_greatest_element_fast set_at_float next_greatest_element)

(def ^:dynamic next_greatest_element_fast_i nil)

(def ^:dynamic next_greatest_element_fast_inner nil)

(def ^:dynamic next_greatest_element_fast_j nil)

(def ^:dynamic next_greatest_element_fast_res nil)

(def ^:dynamic next_greatest_element_i nil)

(def ^:dynamic next_greatest_element_idx nil)

(def ^:dynamic next_greatest_element_k nil)

(def ^:dynamic next_greatest_element_res nil)

(def ^:dynamic next_greatest_element_slow_i nil)

(def ^:dynamic next_greatest_element_slow_j nil)

(def ^:dynamic next_greatest_element_slow_res nil)

(def ^:dynamic next_greatest_element_stack nil)

(def ^:dynamic next_v nil)

(def ^:dynamic set_at_float_i nil)

(def ^:dynamic set_at_float_res nil)

(def ^:dynamic main_arr [(- 10.0) (- 5.0) 0.0 5.0 5.1 11.0 13.0 21.0 3.0 4.0 (- 21.0) (- 10.0) (- 5.0) (- 1.0) 0.0])

(def ^:dynamic main_expected [(- 5.0) 0.0 5.0 5.1 11.0 13.0 21.0 (- 1.0) 4.0 (- 1.0) (- 10.0) (- 5.0) (- 1.0) 0.0 (- 1.0)])

(defn next_greatest_element_slow [next_greatest_element_slow_xs]
  (binding [next_greatest_element_slow_i nil next_greatest_element_slow_j nil next_greatest_element_slow_res nil next_v nil] (try (do (set! next_greatest_element_slow_res []) (set! next_greatest_element_slow_i 0) (while (< next_greatest_element_slow_i (count next_greatest_element_slow_xs)) (do (set! next_v (- 1.0)) (set! next_greatest_element_slow_j (+ next_greatest_element_slow_i 1)) (loop [while_flag_1 true] (when (and while_flag_1 (< next_greatest_element_slow_j (count next_greatest_element_slow_xs))) (cond (< (nth next_greatest_element_slow_xs next_greatest_element_slow_i) (nth next_greatest_element_slow_xs next_greatest_element_slow_j)) (do (set! next_v (nth next_greatest_element_slow_xs next_greatest_element_slow_j)) (recur false)) :else (do (set! next_greatest_element_slow_j (+ next_greatest_element_slow_j 1)) (recur while_flag_1))))) (set! next_greatest_element_slow_res (conj next_greatest_element_slow_res next_v)) (set! next_greatest_element_slow_i (+ next_greatest_element_slow_i 1)))) (throw (ex-info "return" {:v next_greatest_element_slow_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn next_greatest_element_fast [next_greatest_element_fast_xs]
  (binding [next_greatest_element_fast_i nil next_greatest_element_fast_inner nil next_greatest_element_fast_j nil next_greatest_element_fast_res nil next_v nil] (try (do (set! next_greatest_element_fast_res []) (set! next_greatest_element_fast_i 0) (while (< next_greatest_element_fast_i (count next_greatest_element_fast_xs)) (do (set! next_v (- 1.0)) (set! next_greatest_element_fast_j (+ next_greatest_element_fast_i 1)) (loop [while_flag_2 true] (when (and while_flag_2 (< next_greatest_element_fast_j (count next_greatest_element_fast_xs))) (do (set! next_greatest_element_fast_inner (nth next_greatest_element_fast_xs next_greatest_element_fast_j)) (cond (< (nth next_greatest_element_fast_xs next_greatest_element_fast_i) next_greatest_element_fast_inner) (do (set! next_v next_greatest_element_fast_inner) (recur false)) :else (do (set! next_greatest_element_fast_j (+ next_greatest_element_fast_j 1)) (recur while_flag_2)))))) (set! next_greatest_element_fast_res (conj next_greatest_element_fast_res next_v)) (set! next_greatest_element_fast_i (+ next_greatest_element_fast_i 1)))) (throw (ex-info "return" {:v next_greatest_element_fast_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn set_at_float [set_at_float_xs set_at_float_idx set_at_float_value]
  (binding [set_at_float_i nil set_at_float_res nil] (try (do (set! set_at_float_i 0) (set! set_at_float_res []) (while (< set_at_float_i (count set_at_float_xs)) (do (if (= set_at_float_i set_at_float_idx) (set! set_at_float_res (conj set_at_float_res set_at_float_value)) (set! set_at_float_res (conj set_at_float_res (nth set_at_float_xs set_at_float_i)))) (set! set_at_float_i (+ set_at_float_i 1)))) (throw (ex-info "return" {:v set_at_float_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn next_greatest_element [next_greatest_element_xs]
  (binding [next_greatest_element_i nil next_greatest_element_idx nil next_greatest_element_k nil next_greatest_element_res nil next_greatest_element_stack nil] (try (do (set! next_greatest_element_res []) (set! next_greatest_element_k 0) (while (< next_greatest_element_k (count next_greatest_element_xs)) (do (set! next_greatest_element_res (conj next_greatest_element_res (- 1.0))) (set! next_greatest_element_k (+ next_greatest_element_k 1)))) (set! next_greatest_element_stack []) (set! next_greatest_element_i 0) (while (< next_greatest_element_i (count next_greatest_element_xs)) (do (while (and (> (count next_greatest_element_stack) 0) (> (nth next_greatest_element_xs next_greatest_element_i) (nth next_greatest_element_xs (nth next_greatest_element_stack (- (count next_greatest_element_stack) 1))))) (do (set! next_greatest_element_idx (nth next_greatest_element_stack (- (count next_greatest_element_stack) 1))) (set! next_greatest_element_stack (subvec next_greatest_element_stack 0 (- (count next_greatest_element_stack) 1))) (set! next_greatest_element_res (set_at_float next_greatest_element_res next_greatest_element_idx (nth next_greatest_element_xs next_greatest_element_i))))) (set! next_greatest_element_stack (conj next_greatest_element_stack next_greatest_element_i)) (set! next_greatest_element_i (+ next_greatest_element_i 1)))) (throw (ex-info "return" {:v next_greatest_element_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (next_greatest_element_slow main_arr)))
      (println (str (next_greatest_element_fast main_arr)))
      (println (str (next_greatest_element main_arr)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
