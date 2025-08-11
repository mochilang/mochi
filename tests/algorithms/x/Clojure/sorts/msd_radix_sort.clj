(ns main (:refer-clojure :exclude [get_bit_length max_bit_length get_bit _msd_radix_sort msd_radix_sort msd_radix_sort_inplace]))

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

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare get_bit_length max_bit_length get_bit _msd_radix_sort msd_radix_sort msd_radix_sort_inplace)

(def ^:dynamic _msd_radix_sort_i nil)

(def ^:dynamic _msd_radix_sort_num nil)

(def ^:dynamic _msd_radix_sort_ones nil)

(def ^:dynamic _msd_radix_sort_res nil)

(def ^:dynamic _msd_radix_sort_zeros nil)

(def ^:dynamic get_bit_i nil)

(def ^:dynamic get_bit_length_length nil)

(def ^:dynamic get_bit_length_num nil)

(def ^:dynamic get_bit_n nil)

(def ^:dynamic max_bit_length_i nil)

(def ^:dynamic max_bit_length_l nil)

(def ^:dynamic max_bit_length_max_len nil)

(def ^:dynamic msd_radix_sort_bits nil)

(def ^:dynamic msd_radix_sort_i nil)

(def ^:dynamic msd_radix_sort_result nil)

(defn get_bit_length [get_bit_length_n]
  (binding [get_bit_length_length nil get_bit_length_num nil] (try (do (when (= get_bit_length_n 0) (throw (ex-info "return" {:v 1}))) (set! get_bit_length_length 0) (set! get_bit_length_num get_bit_length_n) (while (> get_bit_length_num 0) (do (set! get_bit_length_length (+ get_bit_length_length 1)) (set! get_bit_length_num (/ get_bit_length_num 2)))) (throw (ex-info "return" {:v get_bit_length_length}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn max_bit_length [max_bit_length_nums]
  (binding [max_bit_length_i nil max_bit_length_l nil max_bit_length_max_len nil] (try (do (set! max_bit_length_i 0) (set! max_bit_length_max_len 0) (while (< max_bit_length_i (count max_bit_length_nums)) (do (set! max_bit_length_l (get_bit_length (nth max_bit_length_nums max_bit_length_i))) (when (> max_bit_length_l max_bit_length_max_len) (set! max_bit_length_max_len max_bit_length_l)) (set! max_bit_length_i (+ max_bit_length_i 1)))) (throw (ex-info "return" {:v max_bit_length_max_len}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_bit [get_bit_num get_bit_pos]
  (binding [get_bit_i nil get_bit_n nil] (try (do (set! get_bit_n get_bit_num) (set! get_bit_i 0) (while (< get_bit_i get_bit_pos) (do (set! get_bit_n (/ get_bit_n 2)) (set! get_bit_i (+ get_bit_i 1)))) (throw (ex-info "return" {:v (mod get_bit_n 2)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn _msd_radix_sort [_msd_radix_sort_nums _msd_radix_sort_bit_position]
  (binding [_msd_radix_sort_i nil _msd_radix_sort_num nil _msd_radix_sort_ones nil _msd_radix_sort_res nil _msd_radix_sort_zeros nil] (try (do (when (or (= _msd_radix_sort_bit_position 0) (<= (count _msd_radix_sort_nums) 1)) (throw (ex-info "return" {:v _msd_radix_sort_nums}))) (set! _msd_radix_sort_zeros []) (set! _msd_radix_sort_ones []) (set! _msd_radix_sort_i 0) (while (< _msd_radix_sort_i (count _msd_radix_sort_nums)) (do (set! _msd_radix_sort_num (nth _msd_radix_sort_nums _msd_radix_sort_i)) (if (= (get_bit _msd_radix_sort_num (- _msd_radix_sort_bit_position 1)) 1) (set! _msd_radix_sort_ones (conj _msd_radix_sort_ones _msd_radix_sort_num)) (set! _msd_radix_sort_zeros (conj _msd_radix_sort_zeros _msd_radix_sort_num))) (set! _msd_radix_sort_i (+ _msd_radix_sort_i 1)))) (set! _msd_radix_sort_zeros (_msd_radix_sort _msd_radix_sort_zeros (- _msd_radix_sort_bit_position 1))) (set! _msd_radix_sort_ones (_msd_radix_sort _msd_radix_sort_ones (- _msd_radix_sort_bit_position 1))) (set! _msd_radix_sort_res _msd_radix_sort_zeros) (set! _msd_radix_sort_i 0) (while (< _msd_radix_sort_i (count _msd_radix_sort_ones)) (do (set! _msd_radix_sort_res (conj _msd_radix_sort_res (nth _msd_radix_sort_ones _msd_radix_sort_i))) (set! _msd_radix_sort_i (+ _msd_radix_sort_i 1)))) (throw (ex-info "return" {:v _msd_radix_sort_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn msd_radix_sort [msd_radix_sort_nums]
  (binding [msd_radix_sort_bits nil msd_radix_sort_i nil msd_radix_sort_result nil] (try (do (when (= (count msd_radix_sort_nums) 0) (throw (ex-info "return" {:v []}))) (set! msd_radix_sort_i 0) (while (< msd_radix_sort_i (count msd_radix_sort_nums)) (do (when (< (nth msd_radix_sort_nums msd_radix_sort_i) 0) (throw (Exception. "All numbers must be positive"))) (set! msd_radix_sort_i (+ msd_radix_sort_i 1)))) (set! msd_radix_sort_bits (max_bit_length msd_radix_sort_nums)) (set! msd_radix_sort_result (_msd_radix_sort msd_radix_sort_nums msd_radix_sort_bits)) (throw (ex-info "return" {:v msd_radix_sort_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn msd_radix_sort_inplace [msd_radix_sort_inplace_nums]
  (try (throw (ex-info "return" {:v (msd_radix_sort msd_radix_sort_inplace_nums)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_ex1 [40 12 1 100 4])

(def ^:dynamic main_sorted1 (msd_radix_sort main_ex1))

(def ^:dynamic main_ex2 [])

(def ^:dynamic main_sorted2 (msd_radix_sort main_ex2))

(def ^:dynamic main_ex3 [123 345 123 80])

(def ^:dynamic main_sorted3 (msd_radix_sort main_ex3))

(def ^:dynamic main_ex4 [1209 834598 1 540402 45])

(def ^:dynamic main_sorted4 (msd_radix_sort main_ex4))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str main_sorted1))
      (println (str main_sorted2))
      (println (str main_sorted3))
      (println (str main_sorted4))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
