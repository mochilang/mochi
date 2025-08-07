(ns main (:refer-clojure :exclude [all_rotations sort_strings join_strings bwt_transform index_of reverse_bwt]))

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

(declare all_rotations sort_strings join_strings bwt_transform index_of reverse_bwt)

(def ^:dynamic all_rotations_i nil)

(def ^:dynamic all_rotations_n nil)

(def ^:dynamic all_rotations_rotation nil)

(def ^:dynamic all_rotations_rotations nil)

(def ^:dynamic bwt_transform_bwt_string nil)

(def ^:dynamic bwt_transform_i nil)

(def ^:dynamic bwt_transform_idx nil)

(def ^:dynamic bwt_transform_last_col nil)

(def ^:dynamic bwt_transform_rotations nil)

(def ^:dynamic bwt_transform_word nil)

(def ^:dynamic index_of_i nil)

(def ^:dynamic join_strings_i nil)

(def ^:dynamic join_strings_res nil)

(def ^:dynamic reverse_bwt_ch nil)

(def ^:dynamic reverse_bwt_i nil)

(def ^:dynamic reverse_bwt_iter nil)

(def ^:dynamic reverse_bwt_j nil)

(def ^:dynamic reverse_bwt_n nil)

(def ^:dynamic reverse_bwt_ordered_rotations nil)

(def ^:dynamic sort_strings_arr nil)

(def ^:dynamic sort_strings_i nil)

(def ^:dynamic sort_strings_j nil)

(def ^:dynamic sort_strings_key nil)

(def ^:dynamic sort_strings_n nil)

(defn all_rotations [all_rotations_s]
  (binding [all_rotations_i nil all_rotations_n nil all_rotations_rotation nil all_rotations_rotations nil] (try (do (set! all_rotations_n (count all_rotations_s)) (set! all_rotations_rotations []) (set! all_rotations_i 0) (while (< all_rotations_i all_rotations_n) (do (set! all_rotations_rotation (str (subs all_rotations_s all_rotations_i (min all_rotations_n (count all_rotations_s))) (subs all_rotations_s 0 (min all_rotations_i (count all_rotations_s))))) (set! all_rotations_rotations (conj all_rotations_rotations all_rotations_rotation)) (set! all_rotations_i (+ all_rotations_i 1)))) (throw (ex-info "return" {:v all_rotations_rotations}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sort_strings [sort_strings_arr_p]
  (binding [sort_strings_arr nil sort_strings_i nil sort_strings_j nil sort_strings_key nil sort_strings_n nil] (try (do (set! sort_strings_arr sort_strings_arr_p) (set! sort_strings_n (count sort_strings_arr)) (set! sort_strings_i 1) (while (< sort_strings_i sort_strings_n) (do (set! sort_strings_key (nth sort_strings_arr sort_strings_i)) (set! sort_strings_j (- sort_strings_i 1)) (while (and (>= sort_strings_j 0) (> (compare (nth sort_strings_arr sort_strings_j) sort_strings_key) 0)) (do (set! sort_strings_arr (assoc sort_strings_arr (+ sort_strings_j 1) (nth sort_strings_arr sort_strings_j))) (set! sort_strings_j (- sort_strings_j 1)))) (set! sort_strings_arr (assoc sort_strings_arr (+ sort_strings_j 1) sort_strings_key)) (set! sort_strings_i (+ sort_strings_i 1)))) (throw (ex-info "return" {:v sort_strings_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn join_strings [join_strings_arr]
  (binding [join_strings_i nil join_strings_res nil] (try (do (set! join_strings_res "") (set! join_strings_i 0) (while (< join_strings_i (count join_strings_arr)) (do (set! join_strings_res (str join_strings_res (nth join_strings_arr join_strings_i))) (set! join_strings_i (+ join_strings_i 1)))) (throw (ex-info "return" {:v join_strings_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bwt_transform [bwt_transform_s]
  (binding [bwt_transform_bwt_string nil bwt_transform_i nil bwt_transform_idx nil bwt_transform_last_col nil bwt_transform_rotations nil bwt_transform_word nil] (try (do (when (= bwt_transform_s "") (throw (Exception. "input string must not be empty"))) (set! bwt_transform_rotations (all_rotations bwt_transform_s)) (set! bwt_transform_rotations (sort_strings bwt_transform_rotations)) (set! bwt_transform_last_col []) (set! bwt_transform_i 0) (while (< bwt_transform_i (count bwt_transform_rotations)) (do (set! bwt_transform_word (nth bwt_transform_rotations bwt_transform_i)) (set! bwt_transform_last_col (conj bwt_transform_last_col (subs bwt_transform_word (- (count bwt_transform_word) 1) (min (count bwt_transform_word) (count bwt_transform_word))))) (set! bwt_transform_i (+ bwt_transform_i 1)))) (set! bwt_transform_bwt_string (join_strings bwt_transform_last_col)) (set! bwt_transform_idx (index_of bwt_transform_rotations bwt_transform_s)) (throw (ex-info "return" {:v {:bwt_string bwt_transform_bwt_string :idx_original_string bwt_transform_idx}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn index_of [index_of_arr index_of_target]
  (binding [index_of_i nil] (try (do (set! index_of_i 0) (while (< index_of_i (count index_of_arr)) (do (when (= (nth index_of_arr index_of_i) index_of_target) (throw (ex-info "return" {:v index_of_i}))) (set! index_of_i (+ index_of_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn reverse_bwt [reverse_bwt_bwt_string reverse_bwt_idx_original_string]
  (binding [reverse_bwt_ch nil reverse_bwt_i nil reverse_bwt_iter nil reverse_bwt_j nil reverse_bwt_n nil reverse_bwt_ordered_rotations nil] (try (do (when (= reverse_bwt_bwt_string "") (throw (Exception. "bwt string must not be empty"))) (set! reverse_bwt_n (count reverse_bwt_bwt_string)) (when (or (< reverse_bwt_idx_original_string 0) (>= reverse_bwt_idx_original_string reverse_bwt_n)) (throw (Exception. "index out of range"))) (set! reverse_bwt_ordered_rotations []) (set! reverse_bwt_i 0) (while (< reverse_bwt_i reverse_bwt_n) (do (set! reverse_bwt_ordered_rotations (conj reverse_bwt_ordered_rotations "")) (set! reverse_bwt_i (+ reverse_bwt_i 1)))) (set! reverse_bwt_iter 0) (while (< reverse_bwt_iter reverse_bwt_n) (do (set! reverse_bwt_j 0) (while (< reverse_bwt_j reverse_bwt_n) (do (set! reverse_bwt_ch (subs reverse_bwt_bwt_string reverse_bwt_j (min (+ reverse_bwt_j 1) (count reverse_bwt_bwt_string)))) (set! reverse_bwt_ordered_rotations (assoc reverse_bwt_ordered_rotations reverse_bwt_j (str reverse_bwt_ch (nth reverse_bwt_ordered_rotations reverse_bwt_j)))) (set! reverse_bwt_j (+ reverse_bwt_j 1)))) (set! reverse_bwt_ordered_rotations (sort_strings reverse_bwt_ordered_rotations)) (set! reverse_bwt_iter (+ reverse_bwt_iter 1)))) (throw (ex-info "return" {:v (nth reverse_bwt_ordered_rotations reverse_bwt_idx_original_string)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_s "^BANANA")

(def ^:dynamic main_result (bwt_transform main_s))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (:bwt_string main_result))
      (println (:idx_original_string main_result))
      (println (reverse_bwt (:bwt_string main_result) (:idx_original_string main_result)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
