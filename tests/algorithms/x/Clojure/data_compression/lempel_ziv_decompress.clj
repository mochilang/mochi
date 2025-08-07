(ns main (:refer-clojure :exclude [list_contains is_power_of_two bin_string decompress_data]))

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

(declare list_contains is_power_of_two bin_string decompress_data)

(def ^:dynamic bin_string_bit nil)

(def ^:dynamic bin_string_res nil)

(def ^:dynamic bin_string_x nil)

(def ^:dynamic decompress_data_curr_key nil)

(def ^:dynamic decompress_data_curr_string nil)

(def ^:dynamic decompress_data_i nil)

(def ^:dynamic decompress_data_index nil)

(def ^:dynamic decompress_data_j nil)

(def ^:dynamic decompress_data_keys nil)

(def ^:dynamic decompress_data_last_match_id nil)

(def ^:dynamic decompress_data_lexicon nil)

(def ^:dynamic decompress_data_new_key nil)

(def ^:dynamic decompress_data_new_keys nil)

(def ^:dynamic decompress_data_new_lex nil)

(def ^:dynamic decompress_data_result nil)

(def ^:dynamic is_power_of_two_x nil)

(def ^:dynamic list_contains_i nil)

(defn list_contains [list_contains_xs list_contains_v]
  (binding [list_contains_i nil] (try (do (set! list_contains_i 0) (while (< list_contains_i (count list_contains_xs)) (do (when (= (nth list_contains_xs list_contains_i) list_contains_v) (throw (ex-info "return" {:v true}))) (set! list_contains_i (+ list_contains_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_power_of_two [is_power_of_two_n]
  (binding [is_power_of_two_x nil] (try (do (when (< is_power_of_two_n 1) (throw (ex-info "return" {:v false}))) (set! is_power_of_two_x is_power_of_two_n) (while (> is_power_of_two_x 1) (do (when (not= (mod is_power_of_two_x 2) 0) (throw (ex-info "return" {:v false}))) (set! is_power_of_two_x (quot is_power_of_two_x 2)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bin_string [bin_string_n]
  (binding [bin_string_bit nil bin_string_res nil bin_string_x nil] (try (do (when (= bin_string_n 0) (throw (ex-info "return" {:v "0"}))) (set! bin_string_res "") (set! bin_string_x bin_string_n) (while (> bin_string_x 0) (do (set! bin_string_bit (mod bin_string_x 2)) (set! bin_string_res (str (str bin_string_bit) bin_string_res)) (set! bin_string_x (quot bin_string_x 2)))) (throw (ex-info "return" {:v bin_string_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decompress_data [decompress_data_data_bits]
  (binding [decompress_data_curr_key nil decompress_data_curr_string nil decompress_data_i nil decompress_data_index nil decompress_data_j nil decompress_data_keys nil decompress_data_last_match_id nil decompress_data_lexicon nil decompress_data_new_key nil decompress_data_new_keys nil decompress_data_new_lex nil decompress_data_result nil] (try (do (set! decompress_data_lexicon {"0" "0" "1" "1"}) (set! decompress_data_keys ["0" "1"]) (set! decompress_data_result "") (set! decompress_data_curr_string "") (set! decompress_data_index 2) (set! decompress_data_i 0) (loop [while_flag_1 true] (when (and while_flag_1 (< decompress_data_i (count decompress_data_data_bits))) (do (set! decompress_data_curr_string (str decompress_data_curr_string (subs decompress_data_data_bits decompress_data_i (min (+ decompress_data_i 1) (count decompress_data_data_bits))))) (cond (not (list_contains decompress_data_keys decompress_data_curr_string)) (do (set! decompress_data_i (+ decompress_data_i 1)) (recur true)) :else (do (set! decompress_data_last_match_id (get decompress_data_lexicon decompress_data_curr_string)) (set! decompress_data_result (str decompress_data_result decompress_data_last_match_id)) (set! decompress_data_lexicon (assoc decompress_data_lexicon decompress_data_curr_string (str decompress_data_last_match_id "0"))) (when (is_power_of_two decompress_data_index) (do (set! decompress_data_new_lex {}) (set! decompress_data_new_keys []) (set! decompress_data_j 0) (while (< decompress_data_j (count decompress_data_keys)) (do (set! decompress_data_curr_key (nth decompress_data_keys decompress_data_j)) (set! decompress_data_new_lex (assoc decompress_data_new_lex (str "0" decompress_data_curr_key) (get decompress_data_lexicon decompress_data_curr_key))) (set! decompress_data_new_keys (conj decompress_data_new_keys (str "0" decompress_data_curr_key))) (set! decompress_data_j (+ decompress_data_j 1)))) (set! decompress_data_lexicon decompress_data_new_lex) (set! decompress_data_keys decompress_data_new_keys))) (set! decompress_data_new_key (bin_string decompress_data_index)) (set! decompress_data_lexicon (assoc decompress_data_lexicon decompress_data_new_key (str decompress_data_last_match_id "1"))) (set! decompress_data_keys (conj decompress_data_keys decompress_data_new_key)) (set! decompress_data_index (+ decompress_data_index 1)) (set! decompress_data_curr_string "") (set! decompress_data_i (+ decompress_data_i 1)) (recur while_flag_1)))))) (throw (ex-info "return" {:v decompress_data_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_sample "1011001")

(def ^:dynamic main_decompressed (decompress_data main_sample))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println main_decompressed)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
