(ns main (:refer-clojure :exclude [repeat add_str sub_str mul_digit mul_str pad_left main]))

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

(declare repeat add_str sub_str mul_digit mul_str pad_left main)

(declare _read_file)

(def ^:dynamic add_str_carry nil)

(def ^:dynamic add_str_da nil)

(def ^:dynamic add_str_db nil)

(def ^:dynamic add_str_i nil)

(def ^:dynamic add_str_j nil)

(def ^:dynamic add_str_res nil)

(def ^:dynamic add_str_sum nil)

(def ^:dynamic main__ nil)

(def ^:dynamic main_a nil)

(def ^:dynamic main_b nil)

(def ^:dynamic main_ch nil)

(def ^:dynamic main_dash1 nil)

(def ^:dynamic main_firstPart nil)

(def ^:dynamic main_idx nil)

(def ^:dynamic main_l nil)

(def ^:dynamic main_line nil)

(def ^:dynamic main_op nil)

(def ^:dynamic main_p nil)

(def ^:dynamic main_parts nil)

(def ^:dynamic main_r nil)

(def ^:dynamic main_res nil)

(def ^:dynamic main_secondLen nil)

(def ^:dynamic main_shift nil)

(def ^:dynamic main_spaces nil)

(def ^:dynamic main_t nil)

(def ^:dynamic main_tStr nil)

(def ^:dynamic main_val nil)

(def ^:dynamic main_width nil)

(def ^:dynamic mul_digit_carry nil)

(def ^:dynamic mul_digit_i nil)

(def ^:dynamic mul_digit_k nil)

(def ^:dynamic mul_digit_prod nil)

(def ^:dynamic mul_digit_res nil)

(def ^:dynamic mul_str__ nil)

(def ^:dynamic mul_str_d nil)

(def ^:dynamic mul_str_i nil)

(def ^:dynamic mul_str_part nil)

(def ^:dynamic mul_str_parts nil)

(def ^:dynamic mul_str_result nil)

(def ^:dynamic mul_str_shift nil)

(def ^:dynamic mul_str_shifted nil)

(def ^:dynamic pad_left__ nil)

(def ^:dynamic pad_left_r nil)

(def ^:dynamic repeat__ nil)

(def ^:dynamic repeat_r nil)

(def ^:dynamic sub_str_borrow nil)

(def ^:dynamic sub_str_da nil)

(def ^:dynamic sub_str_db nil)

(def ^:dynamic sub_str_diff nil)

(def ^:dynamic sub_str_i nil)

(def ^:dynamic sub_str_j nil)

(def ^:dynamic sub_str_k nil)

(def ^:dynamic sub_str_res nil)

(def ^:dynamic main_digitMap nil)

(defn repeat [repeat_s repeat_n]
  (binding [repeat__ nil repeat_r nil] (try (do (set! repeat_r "") (dotimes [repeat__ repeat_n] (set! repeat_r (str repeat_r repeat_s))) (throw (ex-info "return" {:v repeat_r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn add_str [add_str_a add_str_b]
  (binding [add_str_carry nil add_str_da nil add_str_db nil add_str_i nil add_str_j nil add_str_res nil add_str_sum nil] (try (do (set! add_str_i (- (count add_str_a) 1)) (set! add_str_j (- (count add_str_b) 1)) (set! add_str_carry 0) (set! add_str_res "") (while (or (or (>= add_str_i 0) (>= add_str_j 0)) (> add_str_carry 0)) (do (set! add_str_da 0) (when (>= add_str_i 0) (set! add_str_da (Long/parseLong (get main_digitMap (subs add_str_a add_str_i (min (+' add_str_i 1) (count add_str_a))))))) (set! add_str_db 0) (when (>= add_str_j 0) (set! add_str_db (Long/parseLong (get main_digitMap (subs add_str_b add_str_j (min (+' add_str_j 1) (count add_str_b))))))) (set! add_str_sum (+' (+' add_str_da add_str_db) add_str_carry)) (set! add_str_res (str (mochi_str (mod add_str_sum 10)) add_str_res)) (set! add_str_carry (/ add_str_sum 10)) (set! add_str_i (- add_str_i 1)) (set! add_str_j (- add_str_j 1)))) (throw (ex-info "return" {:v add_str_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sub_str [sub_str_a sub_str_b]
  (binding [sub_str_borrow nil sub_str_da nil sub_str_db nil sub_str_diff nil sub_str_i nil sub_str_j nil sub_str_k nil sub_str_res nil] (try (do (set! sub_str_i (- (count sub_str_a) 1)) (set! sub_str_j (- (count sub_str_b) 1)) (set! sub_str_borrow 0) (set! sub_str_res "") (while (>= sub_str_i 0) (do (set! sub_str_da (- (Long/parseLong (get main_digitMap (subs sub_str_a sub_str_i (min (+' sub_str_i 1) (count sub_str_a))))) sub_str_borrow)) (set! sub_str_db 0) (when (>= sub_str_j 0) (set! sub_str_db (Long/parseLong (get main_digitMap (subs sub_str_b sub_str_j (min (+' sub_str_j 1) (count sub_str_b))))))) (if (< sub_str_da sub_str_db) (do (set! sub_str_da (+' sub_str_da 10)) (set! sub_str_borrow 1)) (set! sub_str_borrow 0)) (set! sub_str_diff (- sub_str_da sub_str_db)) (set! sub_str_res (str (mochi_str sub_str_diff) sub_str_res)) (set! sub_str_i (- sub_str_i 1)) (set! sub_str_j (- sub_str_j 1)))) (set! sub_str_k 0) (while (and (< sub_str_k (count sub_str_res)) (= (subs sub_str_res sub_str_k (min (+' sub_str_k 1) (count sub_str_res))) "0")) (set! sub_str_k (+' sub_str_k 1))) (if (= sub_str_k (count sub_str_res)) "0" (subs sub_str_res sub_str_k (min (count sub_str_res) (count sub_str_res))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mul_digit [mul_digit_a mul_digit_d]
  (binding [mul_digit_carry nil mul_digit_i nil mul_digit_k nil mul_digit_prod nil mul_digit_res nil] (try (do (when (= mul_digit_d 0) (throw (ex-info "return" {:v "0"}))) (set! mul_digit_i (- (count mul_digit_a) 1)) (set! mul_digit_carry 0) (set! mul_digit_res "") (while (>= mul_digit_i 0) (do (set! mul_digit_prod (+' (*' (Long/parseLong (get main_digitMap (subs mul_digit_a mul_digit_i (min (+' mul_digit_i 1) (count mul_digit_a))))) mul_digit_d) mul_digit_carry)) (set! mul_digit_res (str (mochi_str (mod mul_digit_prod 10)) mul_digit_res)) (set! mul_digit_carry (/ mul_digit_prod 10)) (set! mul_digit_i (- mul_digit_i 1)))) (when (> mul_digit_carry 0) (set! mul_digit_res (str (mochi_str mul_digit_carry) mul_digit_res))) (set! mul_digit_k 0) (while (and (< mul_digit_k (count mul_digit_res)) (= (subs mul_digit_res mul_digit_k (min (+' mul_digit_k 1) (count mul_digit_res))) "0")) (set! mul_digit_k (+' mul_digit_k 1))) (if (= mul_digit_k (count mul_digit_res)) "0" (subs mul_digit_res mul_digit_k (min (count mul_digit_res) (count mul_digit_res))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mul_str [mul_str_a mul_str_b]
  (binding [mul_str__ nil mul_str_d nil mul_str_i nil mul_str_part nil mul_str_parts nil mul_str_result nil mul_str_shift nil mul_str_shifted nil] (try (do (set! mul_str_result "0") (set! mul_str_shift 0) (set! mul_str_parts []) (set! mul_str_i (- (count mul_str_b) 1)) (while (>= mul_str_i 0) (do (set! mul_str_d (Long/parseLong (get main_digitMap (subs mul_str_b mul_str_i (min (+' mul_str_i 1) (count mul_str_b)))))) (set! mul_str_part (mul_digit mul_str_a mul_str_d)) (set! mul_str_parts (conj mul_str_parts {"shift" mul_str_shift "val" mul_str_part})) (set! mul_str_shifted mul_str_part) (dotimes [mul_str__ mul_str_shift] (set! mul_str_shifted (str mul_str_shifted "0"))) (set! mul_str_result (add_str mul_str_result mul_str_shifted)) (set! mul_str_shift (+' mul_str_shift 1)) (set! mul_str_i (- mul_str_i 1)))) (throw (ex-info "return" {:v {"parts" mul_str_parts "res" mul_str_result}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pad_left [pad_left_s pad_left_total]
  (binding [pad_left__ nil pad_left_r nil] (try (do (set! pad_left_r "") (dotimes [pad_left__ (- pad_left_total (count pad_left_s))] (set! pad_left_r (str pad_left_r " "))) (throw (ex-info "return" {:v (str pad_left_r pad_left_s)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main__ nil main_a nil main_b nil main_ch nil main_dash1 nil main_firstPart nil main_idx nil main_l nil main_line nil main_op nil main_p nil main_parts nil main_r nil main_res nil main_secondLen nil main_shift nil main_spaces nil main_t nil main_tStr nil main_val nil main_width nil] (try (do (set! main_tStr (read-line)) (when (= main_tStr "") (throw (ex-info "return" {:v nil}))) (set! main_t (toi main_tStr)) (loop [main___seq (range main_t)] (when (seq main___seq) (let [main__ (first main___seq)] (do (set! main_line (read-line)) (cond (= main_line "") (recur (rest main___seq)) :else (do (set! main_idx 0) (loop [while_flag_1 true] (when (and while_flag_1 (< main_idx (count main_line))) (do (set! main_ch (subs main_line main_idx (min (+' main_idx 1) (count main_line)))) (if (or (or (= main_ch "+") (= main_ch "-")) (= main_ch "*")) (recur nil) (do (set! main_idx (+' main_idx 1)) (recur while_flag_1)))))) (set! main_a (subs main_line 0 (min main_idx (count main_line)))) (set! main_op (subs main_line main_idx (min (+' main_idx 1) (count main_line)))) (set! main_b (subs main_line (+' main_idx 1) (min (count main_line) (count main_line)))) (set! main_res "") (set! main_parts []) (if (= main_op "+") (set! main_res (add_str main_a main_b)) (if (= main_op "-") (set! main_res (sub_str main_a main_b)) (do (set! main_r (mul_str main_a main_b)) (set! main_res (str (get main_r "res"))) (set! main_parts (get main_r "parts"))))) (set! main_width (count main_a)) (set! main_secondLen (+' (count main_b) 1)) (when (> main_secondLen main_width) (set! main_width main_secondLen)) (when (> (count main_res) main_width) (set! main_width (count main_res))) (doseq [main_p main_parts] (do (set! main_l (+' (count (str (get main_p "val"))) (Long/parseLong (get main_p "shift")))) (when (> main_l main_width) (set! main_width main_l)))) (println (pad_left main_a main_width)) (println (pad_left (str main_op main_b) main_width)) (set! main_dash1 0) (if (= main_op "*") (if (> (count main_parts) 0) (do (set! main_dash1 (+' (count main_b) 1)) (set! main_firstPart (str (subs (subs main_parts 0 (+ 0 1)) "val" (+ "val" 1)))) (when (> (count main_firstPart) main_dash1) (set! main_dash1 (count main_firstPart)))) (do (set! main_dash1 (+' (count main_b) 1)) (when (> (count main_res) main_dash1) (set! main_dash1 (count main_res))))) (do (set! main_dash1 (+' (count main_b) 1)) (when (> (count main_res) main_dash1) (set! main_dash1 (count main_res))))) (println (pad_left (repeat "-" main_dash1) main_width)) (when (and (= main_op "*") (> (count main_b) 1)) (do (doseq [main_p main_parts] (do (set! main_val (str (get main_p "val"))) (set! main_shift (Long/parseLong (get main_p "shift"))) (set! main_spaces (- (- main_width main_shift) (count main_val))) (set! main_line "") (dotimes [main__ main_spaces] (set! main_line (str main_line " "))) (set! main_line (str main_line main_val)) (println main_line))) (println (pad_left (repeat "-" (count main_res)) main_width)))) (println (pad_left main_res main_width)) (println "") (recur (rest main___seq))))))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_digitMap) (constantly {"0" 0 "1" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9}))
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
