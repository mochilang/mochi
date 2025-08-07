(ns main (:refer-clojure :exclude [token_to_string tokens_to_string match_length_from_index find_encoding_token lz77_compress lz77_decompress]))

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

(declare token_to_string tokens_to_string match_length_from_index find_encoding_token lz77_compress lz77_decompress)

(def ^:dynamic find_encoding_token_ch nil)

(def ^:dynamic find_encoding_token_found_length nil)

(def ^:dynamic find_encoding_token_found_offset nil)

(def ^:dynamic find_encoding_token_i nil)

(def ^:dynamic find_encoding_token_length nil)

(def ^:dynamic find_encoding_token_offset nil)

(def ^:dynamic lz77_compress_add_len nil)

(def ^:dynamic lz77_compress_output nil)

(def ^:dynamic lz77_compress_remaining nil)

(def ^:dynamic lz77_compress_search_buffer nil)

(def ^:dynamic lz77_compress_search_buffer_size nil)

(def ^:dynamic lz77_compress_token nil)

(def ^:dynamic lz77_decompress_i nil)

(def ^:dynamic lz77_decompress_output nil)

(def ^:dynamic match_length_from_index_tc nil)

(def ^:dynamic match_length_from_index_wc nil)

(def ^:dynamic tokens_to_string_i nil)

(def ^:dynamic tokens_to_string_res nil)

(defn token_to_string [token_to_string_t]
  (try (throw (ex-info "return" {:v (str (str (str (str (str (str "(" (str (:offset token_to_string_t))) ", ") (str (:length token_to_string_t))) ", ") (:indicator token_to_string_t)) ")")})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn tokens_to_string [tokens_to_string_ts]
  (binding [tokens_to_string_i nil tokens_to_string_res nil] (try (do (set! tokens_to_string_res "[") (set! tokens_to_string_i 0) (while (< tokens_to_string_i (count tokens_to_string_ts)) (do (set! tokens_to_string_res (str tokens_to_string_res (token_to_string (nth tokens_to_string_ts tokens_to_string_i)))) (when (< tokens_to_string_i (- (count tokens_to_string_ts) 1)) (set! tokens_to_string_res (str tokens_to_string_res ", "))) (set! tokens_to_string_i (+ tokens_to_string_i 1)))) (throw (ex-info "return" {:v (str tokens_to_string_res "]")}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn match_length_from_index [match_length_from_index_text match_length_from_index_window match_length_from_index_text_index match_length_from_index_window_index]
  (binding [match_length_from_index_tc nil match_length_from_index_wc nil] (try (do (when (or (>= match_length_from_index_text_index (count match_length_from_index_text)) (>= match_length_from_index_window_index (count match_length_from_index_window))) (throw (ex-info "return" {:v 0}))) (set! match_length_from_index_tc (subs match_length_from_index_text match_length_from_index_text_index (min (+ match_length_from_index_text_index 1) (count match_length_from_index_text)))) (set! match_length_from_index_wc (subs match_length_from_index_window match_length_from_index_window_index (min (+ match_length_from_index_window_index 1) (count match_length_from_index_window)))) (if (not= match_length_from_index_tc match_length_from_index_wc) 0 (+ 1 (match_length_from_index match_length_from_index_text (str match_length_from_index_window match_length_from_index_tc) (+ match_length_from_index_text_index 1) (+ match_length_from_index_window_index 1))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_encoding_token [find_encoding_token_text find_encoding_token_search_buffer]
  (binding [find_encoding_token_ch nil find_encoding_token_found_length nil find_encoding_token_found_offset nil find_encoding_token_i nil find_encoding_token_length nil find_encoding_token_offset nil] (try (do (when (= (count find_encoding_token_text) 0) (throw (Exception. "We need some text to work with."))) (set! find_encoding_token_length 0) (set! find_encoding_token_offset 0) (when (= (count find_encoding_token_search_buffer) 0) (throw (ex-info "return" {:v {:offset find_encoding_token_offset :length find_encoding_token_length :indicator (subs find_encoding_token_text 0 (min 1 (count find_encoding_token_text)))}}))) (set! find_encoding_token_i 0) (while (< find_encoding_token_i (count find_encoding_token_search_buffer)) (do (set! find_encoding_token_ch (subs find_encoding_token_search_buffer find_encoding_token_i (min (+ find_encoding_token_i 1) (count find_encoding_token_search_buffer)))) (set! find_encoding_token_found_offset (- (count find_encoding_token_search_buffer) find_encoding_token_i)) (when (= find_encoding_token_ch (subs find_encoding_token_text 0 (min 1 (count find_encoding_token_text)))) (do (set! find_encoding_token_found_length (match_length_from_index find_encoding_token_text find_encoding_token_search_buffer 0 find_encoding_token_i)) (when (>= find_encoding_token_found_length find_encoding_token_length) (do (set! find_encoding_token_offset find_encoding_token_found_offset) (set! find_encoding_token_length find_encoding_token_found_length))))) (set! find_encoding_token_i (+ find_encoding_token_i 1)))) (throw (ex-info "return" {:v {:offset find_encoding_token_offset :length find_encoding_token_length :indicator (subs find_encoding_token_text find_encoding_token_length (min (+ find_encoding_token_length 1) (count find_encoding_token_text)))}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn lz77_compress [lz77_compress_text lz77_compress_window_size lz77_compress_lookahead]
  (binding [lz77_compress_add_len nil lz77_compress_output nil lz77_compress_remaining nil lz77_compress_search_buffer nil lz77_compress_search_buffer_size nil lz77_compress_token nil] (try (do (set! lz77_compress_search_buffer_size (- lz77_compress_window_size lz77_compress_lookahead)) (set! lz77_compress_output []) (set! lz77_compress_search_buffer "") (set! lz77_compress_remaining lz77_compress_text) (while (> (count lz77_compress_remaining) 0) (do (set! lz77_compress_token (find_encoding_token lz77_compress_remaining lz77_compress_search_buffer)) (set! lz77_compress_add_len (+ (:length lz77_compress_token) 1)) (set! lz77_compress_search_buffer (str lz77_compress_search_buffer (subs lz77_compress_remaining 0 (min lz77_compress_add_len (count lz77_compress_remaining))))) (when (> (count lz77_compress_search_buffer) lz77_compress_search_buffer_size) (set! lz77_compress_search_buffer (subs lz77_compress_search_buffer (- (count lz77_compress_search_buffer) lz77_compress_search_buffer_size) (min (count lz77_compress_search_buffer) (count lz77_compress_search_buffer))))) (set! lz77_compress_remaining (subs lz77_compress_remaining lz77_compress_add_len (min (count lz77_compress_remaining) (count lz77_compress_remaining)))) (set! lz77_compress_output (conj lz77_compress_output lz77_compress_token)))) (throw (ex-info "return" {:v lz77_compress_output}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn lz77_decompress [lz77_decompress_tokens]
  (binding [lz77_decompress_i nil lz77_decompress_output nil] (try (do (set! lz77_decompress_output "") (doseq [t lz77_decompress_tokens] (do (set! lz77_decompress_i 0) (while (< lz77_decompress_i (:length t)) (do (set! lz77_decompress_output (str lz77_decompress_output (subs lz77_decompress_output (- (count lz77_decompress_output) (:offset t)) (min (+ (- (count lz77_decompress_output) (:offset t)) 1) (count lz77_decompress_output))))) (set! lz77_decompress_i (+ lz77_decompress_i 1)))) (set! lz77_decompress_output (str lz77_decompress_output (:indicator t))))) (throw (ex-info "return" {:v lz77_decompress_output}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_c1 (lz77_compress "ababcbababaa" 13 6))

(def ^:dynamic main_c2 (lz77_compress "aacaacabcabaaac" 13 6))

(def ^:dynamic main_tokens_example [{:offset 0 :length 0 :indicator "c"} {:offset 0 :length 0 :indicator "a"} {:offset 0 :length 0 :indicator "b"} {:offset 0 :length 0 :indicator "r"} {:offset 3 :length 1 :indicator "c"} {:offset 2 :length 1 :indicator "d"} {:offset 7 :length 4 :indicator "r"} {:offset 3 :length 5 :indicator "d"}])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (tokens_to_string main_c1))
      (println (tokens_to_string main_c2))
      (println (lz77_decompress main_tokens_example))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
