(ns main (:refer-clojure :exclude [index_of is_digit to_lower pad_left alphanum_key compare_keys natural_sort]))

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

(declare index_of is_digit to_lower pad_left alphanum_key compare_keys natural_sort)

(def ^:dynamic alphanum_key_i nil)

(def ^:dynamic alphanum_key_key nil)

(def ^:dynamic alphanum_key_len_str nil)

(def ^:dynamic alphanum_key_num nil)

(def ^:dynamic alphanum_key_seg nil)

(def ^:dynamic compare_keys_i nil)

(def ^:dynamic index_of_i nil)

(def ^:dynamic natural_sort_current nil)

(def ^:dynamic natural_sort_current_key nil)

(def ^:dynamic natural_sort_i nil)

(def ^:dynamic natural_sort_j nil)

(def ^:dynamic natural_sort_k nil)

(def ^:dynamic natural_sort_keys nil)

(def ^:dynamic natural_sort_res nil)

(def ^:dynamic pad_left_res nil)

(def ^:dynamic to_lower_idx nil)

(def ^:dynamic main_DIGITS "0123456789")

(def ^:dynamic main_LOWER "abcdefghijklmnopqrstuvwxyz")

(def ^:dynamic main_UPPER "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn index_of [index_of_s index_of_ch]
  (binding [index_of_i nil] (try (do (set! index_of_i 0) (while (< index_of_i (count index_of_s)) (do (when (= (subs index_of_s index_of_i (+ index_of_i 1)) index_of_ch) (throw (ex-info "return" {:v index_of_i}))) (set! index_of_i (+ index_of_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_digit [is_digit_ch]
  (try (throw (ex-info "return" {:v (>= (index_of main_DIGITS is_digit_ch) 0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn to_lower [to_lower_ch]
  (binding [to_lower_idx nil] (try (do (set! to_lower_idx (index_of main_UPPER to_lower_ch)) (if (>= to_lower_idx 0) (subs main_LOWER to_lower_idx (min (+ to_lower_idx 1) (count main_LOWER))) to_lower_ch)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pad_left [pad_left_s pad_left_width]
  (binding [pad_left_res nil] (try (do (set! pad_left_res pad_left_s) (while (< (count pad_left_res) pad_left_width) (set! pad_left_res (str "0" pad_left_res))) (throw (ex-info "return" {:v pad_left_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn alphanum_key [alphanum_key_s]
  (binding [alphanum_key_i nil alphanum_key_key nil alphanum_key_len_str nil alphanum_key_num nil alphanum_key_seg nil] (try (do (set! alphanum_key_key []) (set! alphanum_key_i 0) (while (< alphanum_key_i (count alphanum_key_s)) (if (is_digit (subs alphanum_key_s alphanum_key_i (+ alphanum_key_i 1))) (do (set! alphanum_key_num "") (while (and (< alphanum_key_i (count alphanum_key_s)) (is_digit (subs alphanum_key_s alphanum_key_i (+ alphanum_key_i 1)))) (do (set! alphanum_key_num (str alphanum_key_num (subs alphanum_key_s alphanum_key_i (+ alphanum_key_i 1)))) (set! alphanum_key_i (+ alphanum_key_i 1)))) (set! alphanum_key_len_str (pad_left (str (count alphanum_key_num)) 3)) (set! alphanum_key_key (conj alphanum_key_key (str (str "#" alphanum_key_len_str) alphanum_key_num)))) (do (set! alphanum_key_seg "") (loop [while_flag_1 true] (when (and while_flag_1 (< alphanum_key_i (count alphanum_key_s))) (cond (is_digit (subs alphanum_key_s alphanum_key_i (+ alphanum_key_i 1))) (recur false) :else (do (set! alphanum_key_seg (str alphanum_key_seg (to_lower (subs alphanum_key_s alphanum_key_i (+ alphanum_key_i 1))))) (set! alphanum_key_i (+ alphanum_key_i 1)) (recur while_flag_1))))) (set! alphanum_key_key (conj alphanum_key_key alphanum_key_seg))))) (throw (ex-info "return" {:v alphanum_key_key}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn compare_keys [compare_keys_a compare_keys_b]
  (binding [compare_keys_i nil] (try (do (set! compare_keys_i 0) (while (and (< compare_keys_i (count compare_keys_a)) (< compare_keys_i (count compare_keys_b))) (do (when (< (compare (nth compare_keys_a compare_keys_i) (nth compare_keys_b compare_keys_i)) 0) (throw (ex-info "return" {:v (- 1)}))) (when (> (compare (nth compare_keys_a compare_keys_i) (nth compare_keys_b compare_keys_i)) 0) (throw (ex-info "return" {:v 1}))) (set! compare_keys_i (+ compare_keys_i 1)))) (when (< (count compare_keys_a) (count compare_keys_b)) (throw (ex-info "return" {:v (- 1)}))) (if (> (count compare_keys_a) (count compare_keys_b)) 1 0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn natural_sort [natural_sort_arr]
  (binding [natural_sort_current nil natural_sort_current_key nil natural_sort_i nil natural_sort_j nil natural_sort_k nil natural_sort_keys nil natural_sort_res nil] (try (do (set! natural_sort_res []) (set! natural_sort_keys []) (set! natural_sort_k 0) (while (< natural_sort_k (count natural_sort_arr)) (do (set! natural_sort_res (conj natural_sort_res (nth natural_sort_arr natural_sort_k))) (set! natural_sort_keys (conj natural_sort_keys (alphanum_key (nth natural_sort_arr natural_sort_k)))) (set! natural_sort_k (+ natural_sort_k 1)))) (set! natural_sort_i 1) (while (< natural_sort_i (count natural_sort_res)) (do (set! natural_sort_current (nth natural_sort_res natural_sort_i)) (set! natural_sort_current_key (nth natural_sort_keys natural_sort_i)) (set! natural_sort_j (- natural_sort_i 1)) (while (and (>= natural_sort_j 0) (> (compare_keys (nth natural_sort_keys natural_sort_j) natural_sort_current_key) 0)) (do (set! natural_sort_res (assoc natural_sort_res (+ natural_sort_j 1) (nth natural_sort_res natural_sort_j))) (set! natural_sort_keys (assoc natural_sort_keys (+ natural_sort_j 1) (nth natural_sort_keys natural_sort_j))) (set! natural_sort_j (- natural_sort_j 1)))) (set! natural_sort_res (assoc natural_sort_res (+ natural_sort_j 1) natural_sort_current)) (set! natural_sort_keys (assoc natural_sort_keys (+ natural_sort_j 1) natural_sort_current_key)) (set! natural_sort_i (+ natural_sort_i 1)))) (throw (ex-info "return" {:v natural_sort_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_example1 ["2 ft 7 in" "1 ft 5 in" "10 ft 2 in" "2 ft 11 in" "7 ft 6 in"])

(def ^:dynamic main_example2 ["Elm11" "Elm12" "Elm2" "elm0" "elm1" "elm10" "elm13" "elm9"])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (natural_sort main_example1)))
      (println (str (natural_sort main_example2)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
