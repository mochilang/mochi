(ns main (:refer-clojure :exclude [max_val min_val counting_sort chr ord counting_sort_string]))

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

(declare max_val min_val counting_sort chr ord counting_sort_string)

(def ^:dynamic counting_sort_coll_len nil)

(def ^:dynamic counting_sort_coll_max nil)

(def ^:dynamic counting_sort_coll_min nil)

(def ^:dynamic counting_sort_counting_arr nil)

(def ^:dynamic counting_sort_counting_arr_length nil)

(def ^:dynamic counting_sort_i nil)

(def ^:dynamic counting_sort_idx nil)

(def ^:dynamic counting_sort_number nil)

(def ^:dynamic counting_sort_ordered nil)

(def ^:dynamic counting_sort_pos nil)

(def ^:dynamic counting_sort_string_codes nil)

(def ^:dynamic counting_sort_string_i nil)

(def ^:dynamic counting_sort_string_res nil)

(def ^:dynamic counting_sort_string_sorted_codes nil)

(def ^:dynamic max_val_i nil)

(def ^:dynamic max_val_m nil)

(def ^:dynamic min_val_i nil)

(def ^:dynamic min_val_m nil)

(def ^:dynamic ord_i nil)

(defn max_val [max_val_arr]
  (binding [max_val_i nil max_val_m nil] (try (do (set! max_val_m (nth max_val_arr 0)) (set! max_val_i 1) (while (< max_val_i (count max_val_arr)) (do (when (> (nth max_val_arr max_val_i) max_val_m) (set! max_val_m (nth max_val_arr max_val_i))) (set! max_val_i (+ max_val_i 1)))) (throw (ex-info "return" {:v max_val_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn min_val [min_val_arr]
  (binding [min_val_i nil min_val_m nil] (try (do (set! min_val_m (nth min_val_arr 0)) (set! min_val_i 1) (while (< min_val_i (count min_val_arr)) (do (when (< (nth min_val_arr min_val_i) min_val_m) (set! min_val_m (nth min_val_arr min_val_i))) (set! min_val_i (+ min_val_i 1)))) (throw (ex-info "return" {:v min_val_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn counting_sort [counting_sort_collection]
  (binding [counting_sort_coll_len nil counting_sort_coll_max nil counting_sort_coll_min nil counting_sort_counting_arr nil counting_sort_counting_arr_length nil counting_sort_i nil counting_sort_idx nil counting_sort_number nil counting_sort_ordered nil counting_sort_pos nil] (try (do (when (= (count counting_sort_collection) 0) (throw (ex-info "return" {:v []}))) (set! counting_sort_coll_len (count counting_sort_collection)) (set! counting_sort_coll_max (max_val counting_sort_collection)) (set! counting_sort_coll_min (min_val counting_sort_collection)) (set! counting_sort_counting_arr_length (- (+ counting_sort_coll_max 1) counting_sort_coll_min)) (set! counting_sort_counting_arr []) (set! counting_sort_i 0) (while (< counting_sort_i counting_sort_counting_arr_length) (do (set! counting_sort_counting_arr (conj counting_sort_counting_arr 0)) (set! counting_sort_i (+ counting_sort_i 1)))) (set! counting_sort_i 0) (while (< counting_sort_i counting_sort_coll_len) (do (set! counting_sort_number (nth counting_sort_collection counting_sort_i)) (set! counting_sort_counting_arr (assoc counting_sort_counting_arr (- counting_sort_number counting_sort_coll_min) (+ (nth counting_sort_counting_arr (- counting_sort_number counting_sort_coll_min)) 1))) (set! counting_sort_i (+ counting_sort_i 1)))) (set! counting_sort_i 1) (while (< counting_sort_i counting_sort_counting_arr_length) (do (set! counting_sort_counting_arr (assoc counting_sort_counting_arr counting_sort_i (+ (nth counting_sort_counting_arr counting_sort_i) (nth counting_sort_counting_arr (- counting_sort_i 1))))) (set! counting_sort_i (+ counting_sort_i 1)))) (set! counting_sort_ordered []) (set! counting_sort_i 0) (while (< counting_sort_i counting_sort_coll_len) (do (set! counting_sort_ordered (conj counting_sort_ordered 0)) (set! counting_sort_i (+ counting_sort_i 1)))) (set! counting_sort_idx (- counting_sort_coll_len 1)) (while (>= counting_sort_idx 0) (do (set! counting_sort_number (nth counting_sort_collection counting_sort_idx)) (set! counting_sort_pos (- (nth counting_sort_counting_arr (- counting_sort_number counting_sort_coll_min)) 1)) (set! counting_sort_ordered (assoc counting_sort_ordered counting_sort_pos counting_sort_number)) (set! counting_sort_counting_arr (assoc counting_sort_counting_arr (- counting_sort_number counting_sort_coll_min) (- (nth counting_sort_counting_arr (- counting_sort_number counting_sort_coll_min)) 1))) (set! counting_sort_idx (- counting_sort_idx 1)))) (throw (ex-info "return" {:v counting_sort_ordered}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_ascii_chars " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

(defn chr [chr_code]
  (try (do (when (= chr_code 10) (throw (ex-info "return" {:v "\n"}))) (when (= chr_code 13) (throw (ex-info "return" {:v "\r"}))) (when (= chr_code 9) (throw (ex-info "return" {:v "\t"}))) (if (and (>= chr_code 32) (< chr_code 127)) (subs main_ascii_chars (- chr_code 32) (min (- chr_code 31) (count main_ascii_chars))) "")) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn ord [ord_ch]
  (binding [ord_i nil] (try (do (when (= ord_ch "\n") (throw (ex-info "return" {:v 10}))) (when (= ord_ch "\r") (throw (ex-info "return" {:v 13}))) (when (= ord_ch "\t") (throw (ex-info "return" {:v 9}))) (set! ord_i 0) (while (< ord_i (count main_ascii_chars)) (do (when (= (subs main_ascii_chars ord_i (min (+ ord_i 1) (count main_ascii_chars))) ord_ch) (throw (ex-info "return" {:v (+ 32 ord_i)}))) (set! ord_i (+ ord_i 1)))) (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn counting_sort_string [counting_sort_string_s]
  (binding [counting_sort_string_codes nil counting_sort_string_i nil counting_sort_string_res nil counting_sort_string_sorted_codes nil] (try (do (set! counting_sort_string_codes []) (set! counting_sort_string_i 0) (while (< counting_sort_string_i (count counting_sort_string_s)) (do (set! counting_sort_string_codes (conj counting_sort_string_codes (ord (subs counting_sort_string_s counting_sort_string_i (min (+ counting_sort_string_i 1) (count counting_sort_string_s)))))) (set! counting_sort_string_i (+ counting_sort_string_i 1)))) (set! counting_sort_string_sorted_codes (counting_sort counting_sort_string_codes)) (set! counting_sort_string_res "") (set! counting_sort_string_i 0) (while (< counting_sort_string_i (count counting_sort_string_sorted_codes)) (do (set! counting_sort_string_res (str counting_sort_string_res (chr (nth counting_sort_string_sorted_codes counting_sort_string_i)))) (set! counting_sort_string_i (+ counting_sort_string_i 1)))) (throw (ex-info "return" {:v counting_sort_string_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_example1 (counting_sort [0 5 3 2 2]))

(def ^:dynamic main_example2 (counting_sort []))

(def ^:dynamic main_example3 (counting_sort [(- 2) (- 5) (- 45)]))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str main_example1))
      (println (str main_example2))
      (println (str main_example3))
      (println (counting_sort_string "thisisthestring"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
