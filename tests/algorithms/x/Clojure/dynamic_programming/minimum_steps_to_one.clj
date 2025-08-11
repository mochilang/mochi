(ns main (:refer-clojure :exclude [make_list min_int min_steps_to_one]))

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

(declare make_list min_int min_steps_to_one)

(def ^:dynamic make_list_arr nil)

(def ^:dynamic make_list_i nil)

(def ^:dynamic min_steps_to_one_i nil)

(def ^:dynamic min_steps_to_one_table nil)

(defn make_list [make_list_len make_list_value]
  (binding [make_list_arr nil make_list_i nil] (try (do (set! make_list_arr []) (set! make_list_i 0) (while (< make_list_i make_list_len) (do (set! make_list_arr (conj make_list_arr make_list_value)) (set! make_list_i (+ make_list_i 1)))) (throw (ex-info "return" {:v make_list_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn min_int [min_int_a min_int_b]
  (try (if (< min_int_a min_int_b) min_int_a min_int_b) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn min_steps_to_one [min_steps_to_one_number]
  (binding [min_steps_to_one_i nil min_steps_to_one_table nil] (try (do (when (<= min_steps_to_one_number 0) (throw (ex-info "return" {:v 0}))) (set! min_steps_to_one_table (make_list (+ min_steps_to_one_number 1) (+ min_steps_to_one_number 1))) (set! min_steps_to_one_table (assoc min_steps_to_one_table 1 0)) (set! min_steps_to_one_i 1) (while (< min_steps_to_one_i min_steps_to_one_number) (do (set! min_steps_to_one_table (assoc min_steps_to_one_table (+ min_steps_to_one_i 1) (min_int (nth min_steps_to_one_table (+ min_steps_to_one_i 1)) (+ (nth min_steps_to_one_table min_steps_to_one_i) 1)))) (when (<= (* min_steps_to_one_i 2) min_steps_to_one_number) (set! min_steps_to_one_table (assoc min_steps_to_one_table (* min_steps_to_one_i 2) (min_int (nth min_steps_to_one_table (* min_steps_to_one_i 2)) (+ (nth min_steps_to_one_table min_steps_to_one_i) 1))))) (when (<= (* min_steps_to_one_i 3) min_steps_to_one_number) (set! min_steps_to_one_table (assoc min_steps_to_one_table (* min_steps_to_one_i 3) (min_int (nth min_steps_to_one_table (* min_steps_to_one_i 3)) (+ (nth min_steps_to_one_table min_steps_to_one_i) 1))))) (set! min_steps_to_one_i (+ min_steps_to_one_i 1)))) (throw (ex-info "return" {:v (nth min_steps_to_one_table min_steps_to_one_number)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (min_steps_to_one 10)))
      (println (str (min_steps_to_one 15)))
      (println (str (min_steps_to_one 6)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
