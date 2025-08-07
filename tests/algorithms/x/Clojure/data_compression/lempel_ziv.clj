(ns main (:refer-clojure :exclude [to_binary contains_key_int lzw_compress]))

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

(declare to_binary contains_key_int lzw_compress)

(def ^:dynamic lzw_compress_candidate nil)

(def ^:dynamic lzw_compress_ch nil)

(def ^:dynamic lzw_compress_current nil)

(def ^:dynamic lzw_compress_dict nil)

(def ^:dynamic lzw_compress_i nil)

(def ^:dynamic lzw_compress_index nil)

(def ^:dynamic lzw_compress_result nil)

(def ^:dynamic to_binary_bit nil)

(def ^:dynamic to_binary_num nil)

(def ^:dynamic to_binary_res nil)

(defn to_binary [to_binary_n]
  (binding [to_binary_bit nil to_binary_num nil to_binary_res nil] (try (do (when (= to_binary_n 0) (throw (ex-info "return" {:v "0"}))) (set! to_binary_num to_binary_n) (set! to_binary_res "") (while (> to_binary_num 0) (do (set! to_binary_bit (mod to_binary_num 2)) (set! to_binary_res (str (str to_binary_bit) to_binary_res)) (set! to_binary_num (quot to_binary_num 2)))) (throw (ex-info "return" {:v to_binary_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn contains_key_int [contains_key_int_m contains_key_int_key]
  (try (do (doseq [k (keys contains_key_int_m)] (when (= k contains_key_int_key) (throw (ex-info "return" {:v true})))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn lzw_compress [lzw_compress_bits]
  (binding [lzw_compress_candidate nil lzw_compress_ch nil lzw_compress_current nil lzw_compress_dict nil lzw_compress_i nil lzw_compress_index nil lzw_compress_result nil] (try (do (set! lzw_compress_dict {"0" 0 "1" 1}) (set! lzw_compress_current "") (set! lzw_compress_result "") (set! lzw_compress_index 2) (set! lzw_compress_i 0) (while (< lzw_compress_i (count lzw_compress_bits)) (do (set! lzw_compress_ch (nth lzw_compress_bits lzw_compress_i)) (set! lzw_compress_candidate (str lzw_compress_current lzw_compress_ch)) (if (contains_key_int lzw_compress_dict lzw_compress_candidate) (set! lzw_compress_current lzw_compress_candidate) (do (set! lzw_compress_result (str lzw_compress_result (to_binary (get lzw_compress_dict lzw_compress_current)))) (set! lzw_compress_dict (assoc lzw_compress_dict lzw_compress_candidate lzw_compress_index)) (set! lzw_compress_index (+ lzw_compress_index 1)) (set! lzw_compress_current lzw_compress_ch))) (set! lzw_compress_i (+ lzw_compress_i 1)))) (when (not= lzw_compress_current "") (set! lzw_compress_result (str lzw_compress_result (to_binary (get lzw_compress_dict lzw_compress_current))))) (throw (ex-info "return" {:v lzw_compress_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_data "01001100100111")

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (lzw_compress main_data))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
