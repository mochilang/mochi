(ns main (:refer-clojure :exclude [make_list int_sqrt minimum_squares_to_represent_a_number]))

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

(declare make_list int_sqrt minimum_squares_to_represent_a_number)

(def ^:dynamic int_sqrt_r nil)

(def ^:dynamic make_list_arr nil)

(def ^:dynamic make_list_i nil)

(def ^:dynamic minimum_squares_to_represent_a_number_answer nil)

(def ^:dynamic minimum_squares_to_represent_a_number_answers nil)

(def ^:dynamic minimum_squares_to_represent_a_number_current_answer nil)

(def ^:dynamic minimum_squares_to_represent_a_number_i nil)

(def ^:dynamic minimum_squares_to_represent_a_number_j nil)

(def ^:dynamic minimum_squares_to_represent_a_number_root nil)

(defn make_list [make_list_len make_list_value]
  (binding [make_list_arr nil make_list_i nil] (try (do (set! make_list_arr []) (set! make_list_i 0) (while (< make_list_i make_list_len) (do (set! make_list_arr (conj make_list_arr make_list_value)) (set! make_list_i (+ make_list_i 1)))) (throw (ex-info "return" {:v make_list_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn int_sqrt [int_sqrt_n]
  (binding [int_sqrt_r nil] (try (do (set! int_sqrt_r 0) (while (<= (* (+ int_sqrt_r 1) (+ int_sqrt_r 1)) int_sqrt_n) (set! int_sqrt_r (+ int_sqrt_r 1))) (throw (ex-info "return" {:v int_sqrt_r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn minimum_squares_to_represent_a_number [minimum_squares_to_represent_a_number_number]
  (binding [minimum_squares_to_represent_a_number_answer nil minimum_squares_to_represent_a_number_answers nil minimum_squares_to_represent_a_number_current_answer nil minimum_squares_to_represent_a_number_i nil minimum_squares_to_represent_a_number_j nil minimum_squares_to_represent_a_number_root nil] (try (do (when (< minimum_squares_to_represent_a_number_number 0) (throw (Exception. "the value of input must not be a negative number"))) (when (= minimum_squares_to_represent_a_number_number 0) (throw (ex-info "return" {:v 1}))) (set! minimum_squares_to_represent_a_number_answers (make_list (+ minimum_squares_to_represent_a_number_number 1) (- 1))) (set! minimum_squares_to_represent_a_number_answers (assoc minimum_squares_to_represent_a_number_answers 0 0)) (set! minimum_squares_to_represent_a_number_i 1) (while (<= minimum_squares_to_represent_a_number_i minimum_squares_to_represent_a_number_number) (do (set! minimum_squares_to_represent_a_number_answer minimum_squares_to_represent_a_number_i) (set! minimum_squares_to_represent_a_number_root (int_sqrt minimum_squares_to_represent_a_number_i)) (set! minimum_squares_to_represent_a_number_j 1) (while (<= minimum_squares_to_represent_a_number_j minimum_squares_to_represent_a_number_root) (do (set! minimum_squares_to_represent_a_number_current_answer (+ 1 (nth minimum_squares_to_represent_a_number_answers (- minimum_squares_to_represent_a_number_i (* minimum_squares_to_represent_a_number_j minimum_squares_to_represent_a_number_j))))) (when (< minimum_squares_to_represent_a_number_current_answer minimum_squares_to_represent_a_number_answer) (set! minimum_squares_to_represent_a_number_answer minimum_squares_to_represent_a_number_current_answer)) (set! minimum_squares_to_represent_a_number_j (+ minimum_squares_to_represent_a_number_j 1)))) (set! minimum_squares_to_represent_a_number_answers (assoc minimum_squares_to_represent_a_number_answers minimum_squares_to_represent_a_number_i minimum_squares_to_represent_a_number_answer)) (set! minimum_squares_to_represent_a_number_i (+ minimum_squares_to_represent_a_number_i 1)))) (throw (ex-info "return" {:v (nth minimum_squares_to_represent_a_number_answers minimum_squares_to_represent_a_number_number)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (minimum_squares_to_represent_a_number 25))
      (println (minimum_squares_to_represent_a_number 21))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
