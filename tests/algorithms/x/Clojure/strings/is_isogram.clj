(ns main (:refer-clojure :exclude [index_of ord chr to_lower_char is_alpha is_isogram]))

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

(declare index_of ord chr to_lower_char is_alpha is_isogram)

(def ^:dynamic chr_lower nil)

(def ^:dynamic chr_upper nil)

(def ^:dynamic index_of_i nil)

(def ^:dynamic is_alpha_code nil)

(def ^:dynamic is_isogram_ch nil)

(def ^:dynamic is_isogram_i nil)

(def ^:dynamic is_isogram_lower nil)

(def ^:dynamic is_isogram_seen nil)

(def ^:dynamic ord_idx nil)

(def ^:dynamic ord_lower nil)

(def ^:dynamic ord_upper nil)

(def ^:dynamic to_lower_char_code nil)

(defn index_of [index_of_s index_of_ch]
  (binding [index_of_i nil] (try (do (set! index_of_i 0) (while (< index_of_i (count index_of_s)) (do (when (= (subs index_of_s index_of_i (+ index_of_i 1)) index_of_ch) (throw (ex-info "return" {:v index_of_i}))) (set! index_of_i (+ index_of_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn ord [ord_ch]
  (binding [ord_idx nil ord_lower nil ord_upper nil] (try (do (set! ord_upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! ord_lower "abcdefghijklmnopqrstuvwxyz") (set! ord_idx (index_of ord_upper ord_ch)) (when (>= ord_idx 0) (throw (ex-info "return" {:v (+ 65 ord_idx)}))) (set! ord_idx (index_of ord_lower ord_ch)) (if (>= ord_idx 0) (+ 97 ord_idx) (- 1))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn chr [chr_n]
  (binding [chr_lower nil chr_upper nil] (try (do (set! chr_upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! chr_lower "abcdefghijklmnopqrstuvwxyz") (when (and (>= chr_n 65) (< chr_n 91)) (throw (ex-info "return" {:v (subs chr_upper (- chr_n 65) (min (- chr_n 64) (count chr_upper)))}))) (if (and (>= chr_n 97) (< chr_n 123)) (subs chr_lower (- chr_n 97) (min (- chr_n 96) (count chr_lower))) "?")) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn to_lower_char [to_lower_char_c]
  (binding [to_lower_char_code nil] (try (do (set! to_lower_char_code (ord to_lower_char_c)) (if (and (>= to_lower_char_code 65) (<= to_lower_char_code 90)) (chr (+ to_lower_char_code 32)) to_lower_char_c)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_alpha [is_alpha_c]
  (binding [is_alpha_code nil] (try (do (set! is_alpha_code (ord is_alpha_c)) (throw (ex-info "return" {:v (or (and (>= is_alpha_code 65) (<= is_alpha_code 90)) (and (>= is_alpha_code 97) (<= is_alpha_code 122)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_isogram [is_isogram_s]
  (binding [is_isogram_ch nil is_isogram_i nil is_isogram_lower nil is_isogram_seen nil] (try (do (set! is_isogram_seen "") (set! is_isogram_i 0) (while (< is_isogram_i (count is_isogram_s)) (do (set! is_isogram_ch (subs is_isogram_s is_isogram_i (+ is_isogram_i 1))) (when (not (is_alpha is_isogram_ch)) (throw (Exception. "String must only contain alphabetic characters."))) (set! is_isogram_lower (to_lower_char is_isogram_ch)) (when (>= (index_of is_isogram_seen is_isogram_lower) 0) (throw (ex-info "return" {:v false}))) (set! is_isogram_seen (str is_isogram_seen is_isogram_lower)) (set! is_isogram_i (+ is_isogram_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (is_isogram "Uncopyrightable")))
      (println (str (is_isogram "allowance")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
