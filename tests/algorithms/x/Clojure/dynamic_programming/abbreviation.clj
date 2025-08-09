(ns main (:refer-clojure :exclude [index_of ord chr to_upper_char is_lower abbr print_bool]))

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

(declare index_of ord chr to_upper_char is_lower abbr print_bool)

(def ^:dynamic abbr_dp nil)

(def ^:dynamic abbr_i nil)

(def ^:dynamic abbr_j nil)

(def ^:dynamic abbr_m nil)

(def ^:dynamic abbr_n nil)

(def ^:dynamic abbr_row nil)

(def ^:dynamic chr_lower nil)

(def ^:dynamic chr_upper nil)

(def ^:dynamic index_of_i nil)

(def ^:dynamic is_lower_code nil)

(def ^:dynamic ord_idx nil)

(def ^:dynamic ord_lower nil)

(def ^:dynamic ord_upper nil)

(def ^:dynamic to_upper_char_code nil)

(defn index_of [index_of_s index_of_ch]
  (binding [index_of_i nil] (try (do (set! index_of_i 0) (while (< index_of_i (count index_of_s)) (do (when (= (subs index_of_s index_of_i (+ index_of_i 1)) index_of_ch) (throw (ex-info "return" {:v index_of_i}))) (set! index_of_i (+ index_of_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn ord [ord_ch]
  (binding [ord_idx nil ord_lower nil ord_upper nil] (try (do (set! ord_upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! ord_lower "abcdefghijklmnopqrstuvwxyz") (set! ord_idx (index_of ord_upper ord_ch)) (when (>= ord_idx 0) (throw (ex-info "return" {:v (+ 65 ord_idx)}))) (set! ord_idx (index_of ord_lower ord_ch)) (if (>= ord_idx 0) (+ 97 ord_idx) 0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn chr [chr_n]
  (binding [chr_lower nil chr_upper nil] (try (do (set! chr_upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! chr_lower "abcdefghijklmnopqrstuvwxyz") (when (and (>= chr_n 65) (< chr_n 91)) (throw (ex-info "return" {:v (subs chr_upper (- chr_n 65) (min (- chr_n 64) (count chr_upper)))}))) (if (and (>= chr_n 97) (< chr_n 123)) (subs chr_lower (- chr_n 97) (min (- chr_n 96) (count chr_lower))) "?")) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn to_upper_char [to_upper_char_c]
  (binding [to_upper_char_code nil] (try (do (set! to_upper_char_code (ord to_upper_char_c)) (if (and (>= to_upper_char_code 97) (<= to_upper_char_code 122)) (chr (- to_upper_char_code 32)) to_upper_char_c)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_lower [is_lower_c]
  (binding [is_lower_code nil] (try (do (set! is_lower_code (ord is_lower_c)) (throw (ex-info "return" {:v (and (>= is_lower_code 97) (<= is_lower_code 122))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn abbr [abbr_a abbr_b]
  (binding [abbr_dp nil abbr_i nil abbr_j nil abbr_m nil abbr_n nil abbr_row nil] (try (do (set! abbr_n (count abbr_a)) (set! abbr_m (count abbr_b)) (set! abbr_dp []) (set! abbr_i 0) (while (<= abbr_i abbr_n) (do (set! abbr_row []) (set! abbr_j 0) (while (<= abbr_j abbr_m) (do (set! abbr_row (conj abbr_row false)) (set! abbr_j (+ abbr_j 1)))) (set! abbr_dp (conj abbr_dp abbr_row)) (set! abbr_i (+ abbr_i 1)))) (set! abbr_dp (assoc-in abbr_dp [0 0] true)) (set! abbr_i 0) (while (< abbr_i abbr_n) (do (set! abbr_j 0) (while (<= abbr_j abbr_m) (do (when (nth (nth abbr_dp abbr_i) abbr_j) (do (when (and (< abbr_j abbr_m) (= (to_upper_char (subs abbr_a abbr_i (+ abbr_i 1))) (subs abbr_b abbr_j (+ abbr_j 1)))) (set! abbr_dp (assoc-in abbr_dp [(+ abbr_i 1) (+ abbr_j 1)] true))) (when (is_lower (subs abbr_a abbr_i (+ abbr_i 1))) (set! abbr_dp (assoc-in abbr_dp [(+ abbr_i 1) abbr_j] true))))) (set! abbr_j (+ abbr_j 1)))) (set! abbr_i (+ abbr_i 1)))) (throw (ex-info "return" {:v (nth (nth abbr_dp abbr_n) abbr_m)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_bool [print_bool_b]
  (do (if print_bool_b (println true) (println false)) print_bool_b))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (print_bool (abbr "daBcd" "ABC"))
      (print_bool (abbr "dBcd" "ABC"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
