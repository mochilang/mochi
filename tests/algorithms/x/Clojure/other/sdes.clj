(ns main (:refer-clojure :exclude [apply_table left_shift xor int_to_binary pad_left bin_to_int apply_sbox f]))

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
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare apply_table left_shift xor int_to_binary pad_left bin_to_int apply_sbox f)

(declare _read_file)

(def ^:dynamic apply_sbox_col nil)

(def ^:dynamic apply_sbox_col_bits nil)

(def ^:dynamic apply_sbox_out nil)

(def ^:dynamic apply_sbox_row nil)

(def ^:dynamic apply_sbox_row_bits nil)

(def ^:dynamic apply_sbox_val nil)

(def ^:dynamic apply_table_i nil)

(def ^:dynamic apply_table_idx nil)

(def ^:dynamic apply_table_res nil)

(def ^:dynamic bin_to_int_digit nil)

(def ^:dynamic bin_to_int_i nil)

(def ^:dynamic bin_to_int_result nil)

(def ^:dynamic f_left nil)

(def ^:dynamic f_left_bin_str nil)

(def ^:dynamic f_right nil)

(def ^:dynamic f_right_bin_str nil)

(def ^:dynamic f_temp nil)

(def ^:dynamic int_to_binary_num nil)

(def ^:dynamic int_to_binary_res nil)

(def ^:dynamic main_left nil)

(def ^:dynamic main_right nil)

(def ^:dynamic main_temp nil)

(def ^:dynamic pad_left_res nil)

(def ^:dynamic xor_i nil)

(def ^:dynamic xor_res nil)

(defn apply_table [apply_table_inp apply_table_table]
  (binding [apply_table_i nil apply_table_idx nil apply_table_res nil] (try (do (set! apply_table_res "") (set! apply_table_i 0) (while (< apply_table_i (count apply_table_table)) (do (set! apply_table_idx (- (nth apply_table_table apply_table_i) 1)) (when (< apply_table_idx 0) (set! apply_table_idx (- (count apply_table_inp) 1))) (set! apply_table_res (str apply_table_res (subs apply_table_inp apply_table_idx (min (+' apply_table_idx 1) (count apply_table_inp))))) (set! apply_table_i (+' apply_table_i 1)))) (throw (ex-info "return" {:v apply_table_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn left_shift [left_shift_data]
  (try (throw (ex-info "return" {:v (str (subs left_shift_data 1 (min (count left_shift_data) (count left_shift_data))) (subs left_shift_data 0 (min 1 (count left_shift_data))))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn xor [xor_a xor_b]
  (binding [xor_i nil xor_res nil] (try (do (set! xor_res "") (set! xor_i 0) (while (and (< xor_i (count xor_a)) (< xor_i (count xor_b))) (do (if (= (subs xor_a xor_i (min (+' xor_i 1) (count xor_a))) (subs xor_b xor_i (min (+' xor_i 1) (count xor_b)))) (set! xor_res (str xor_res "0")) (set! xor_res (str xor_res "1"))) (set! xor_i (+' xor_i 1)))) (throw (ex-info "return" {:v xor_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn int_to_binary [int_to_binary_n]
  (binding [int_to_binary_num nil int_to_binary_res nil] (try (do (when (= int_to_binary_n 0) (throw (ex-info "return" {:v "0"}))) (set! int_to_binary_res "") (set! int_to_binary_num int_to_binary_n) (while (> int_to_binary_num 0) (do (set! int_to_binary_res (str (mochi_str (mod int_to_binary_num 2)) int_to_binary_res)) (set! int_to_binary_num (/ int_to_binary_num 2)))) (throw (ex-info "return" {:v int_to_binary_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pad_left [pad_left_s pad_left_width]
  (binding [pad_left_res nil] (try (do (set! pad_left_res pad_left_s) (while (< (count pad_left_res) pad_left_width) (set! pad_left_res (str "0" pad_left_res))) (throw (ex-info "return" {:v pad_left_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bin_to_int [bin_to_int_s]
  (binding [bin_to_int_digit nil bin_to_int_i nil bin_to_int_result nil] (try (do (set! bin_to_int_result 0) (set! bin_to_int_i 0) (while (< bin_to_int_i (count bin_to_int_s)) (do (set! bin_to_int_digit (Long/parseLong (subs bin_to_int_s bin_to_int_i (min (+' bin_to_int_i 1) (count bin_to_int_s))))) (set! bin_to_int_result (+' (*' bin_to_int_result 2) bin_to_int_digit)) (set! bin_to_int_i (+' bin_to_int_i 1)))) (throw (ex-info "return" {:v bin_to_int_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn apply_sbox [apply_sbox_s apply_sbox_data]
  (binding [apply_sbox_col nil apply_sbox_col_bits nil apply_sbox_out nil apply_sbox_row nil apply_sbox_row_bits nil apply_sbox_val nil] (try (do (set! apply_sbox_row_bits (str (subs apply_sbox_data 0 (min 1 (count apply_sbox_data))) (subs apply_sbox_data (- (count apply_sbox_data) 1) (min (count apply_sbox_data) (count apply_sbox_data))))) (set! apply_sbox_col_bits (subs apply_sbox_data 1 (min 3 (count apply_sbox_data)))) (set! apply_sbox_row (bin_to_int apply_sbox_row_bits)) (set! apply_sbox_col (bin_to_int apply_sbox_col_bits)) (set! apply_sbox_val (nth (nth apply_sbox_s apply_sbox_row) apply_sbox_col)) (set! apply_sbox_out (int_to_binary apply_sbox_val)) (throw (ex-info "return" {:v apply_sbox_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_p4_table nil)

(defn f [f_expansion f_s0 f_s1 f_key f_message]
  (binding [f_left nil f_left_bin_str nil f_right nil f_right_bin_str nil f_temp nil] (try (do (set! f_left (subs f_message 0 (min 4 (count f_message)))) (set! f_right (subs f_message 4 (min 8 (count f_message)))) (set! f_temp (apply_table f_right f_expansion)) (set! f_temp (xor f_temp f_key)) (set! f_left_bin_str (apply_sbox f_s0 (subs f_temp 0 (min 4 (count f_temp))))) (set! f_right_bin_str (apply_sbox f_s1 (subs f_temp 4 (min 8 (count f_temp))))) (set! f_left_bin_str (pad_left f_left_bin_str 2)) (set! f_right_bin_str (pad_left f_right_bin_str 2)) (set! f_temp (apply_table (str f_left_bin_str f_right_bin_str) main_p4_table)) (set! f_temp (xor f_left f_temp)) (throw (ex-info "return" {:v (str f_temp f_right)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_key nil)

(def ^:dynamic main_message nil)

(def ^:dynamic main_p8_table nil)

(def ^:dynamic main_p10_table nil)

(def ^:dynamic main_IP nil)

(def ^:dynamic main_IP_inv nil)

(def ^:dynamic main_expansion nil)

(def ^:dynamic main_s0 nil)

(def ^:dynamic main_s1 nil)

(def ^:dynamic main_temp nil)

(def ^:dynamic main_left nil)

(def ^:dynamic main_right nil)

(def ^:dynamic main_key1 nil)

(def ^:dynamic main_key2 nil)

(def ^:dynamic main_CT nil)

(def ^:dynamic main_PT nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_p4_table) (constantly [2 4 3 1]))
      (alter-var-root (var main_key) (constantly "1010000010"))
      (alter-var-root (var main_message) (constantly "11010111"))
      (alter-var-root (var main_p8_table) (constantly [6 3 7 4 8 5 10 9]))
      (alter-var-root (var main_p10_table) (constantly [3 5 2 7 4 10 1 9 8 6]))
      (alter-var-root (var main_IP) (constantly [2 6 3 1 4 8 5 7]))
      (alter-var-root (var main_IP_inv) (constantly [4 1 3 5 7 2 8 6]))
      (alter-var-root (var main_expansion) (constantly [4 1 2 3 2 3 4 1]))
      (alter-var-root (var main_s0) (constantly [[1 0 3 2] [3 2 1 0] [0 2 1 3] [3 1 3 2]]))
      (alter-var-root (var main_s1) (constantly [[0 1 2 3] [2 0 1 3] [3 0 1 0] [2 1 0 3]]))
      (alter-var-root (var main_temp) (constantly (apply_table main_key main_p10_table)))
      (alter-var-root (var main_left) (constantly (subs main_temp 0 (min 5 (count main_temp)))))
      (alter-var-root (var main_right) (constantly (subs main_temp 5 (min 10 (count main_temp)))))
      (alter-var-root (var main_left) (constantly (left_shift main_left)))
      (alter-var-root (var main_right) (constantly (left_shift main_right)))
      (alter-var-root (var main_key1) (constantly (apply_table (str main_left main_right) main_p8_table)))
      (alter-var-root (var main_left) (constantly (left_shift main_left)))
      (alter-var-root (var main_right) (constantly (left_shift main_right)))
      (alter-var-root (var main_left) (constantly (left_shift main_left)))
      (alter-var-root (var main_right) (constantly (left_shift main_right)))
      (alter-var-root (var main_key2) (constantly (apply_table (str main_left main_right) main_p8_table)))
      (alter-var-root (var main_temp) (constantly (apply_table main_message main_IP)))
      (alter-var-root (var main_temp) (constantly (f main_expansion main_s0 main_s1 main_key1 main_temp)))
      (alter-var-root (var main_temp) (constantly (str (subs main_temp 4 (min 8 (count main_temp))) (subs main_temp 0 (min 4 (count main_temp))))))
      (alter-var-root (var main_temp) (constantly (f main_expansion main_s0 main_s1 main_key2 main_temp)))
      (alter-var-root (var main_CT) (constantly (apply_table main_temp main_IP_inv)))
      (println (str "Cipher text is: " main_CT))
      (alter-var-root (var main_temp) (constantly (apply_table main_CT main_IP)))
      (alter-var-root (var main_temp) (constantly (f main_expansion main_s0 main_s1 main_key2 main_temp)))
      (alter-var-root (var main_temp) (constantly (str (subs main_temp 4 (min 8 (count main_temp))) (subs main_temp 0 (min 4 (count main_temp))))))
      (alter-var-root (var main_temp) (constantly (f main_expansion main_s0 main_s1 main_key1 main_temp)))
      (alter-var-root (var main_PT) (constantly (apply_table main_temp main_IP_inv)))
      (println (str "Plain text after decypting is: " main_PT))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
