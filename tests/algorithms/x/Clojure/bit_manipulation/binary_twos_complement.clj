(ns main (:refer-clojure :exclude [repeat_char to_binary pow2 twos_complement]))

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

(declare repeat_char to_binary pow2 twos_complement)

(def ^:dynamic pow2_i nil)

(def ^:dynamic pow2_res nil)

(def ^:dynamic repeat_char_i nil)

(def ^:dynamic repeat_char_res nil)

(def ^:dynamic to_binary_res nil)

(def ^:dynamic to_binary_v nil)

(def ^:dynamic twos_complement_abs_number nil)

(def ^:dynamic twos_complement_binary_number_length nil)

(def ^:dynamic twos_complement_complement_binary nil)

(def ^:dynamic twos_complement_complement_value nil)

(def ^:dynamic twos_complement_padding nil)

(def ^:dynamic twos_complement_twos_complement_number nil)

(defn repeat_char [repeat_char_ch repeat_char_times]
  (binding [repeat_char_i nil repeat_char_res nil] (try (do (set! repeat_char_res "") (set! repeat_char_i 0) (while (< repeat_char_i repeat_char_times) (do (set! repeat_char_res (str repeat_char_res repeat_char_ch)) (set! repeat_char_i (+ repeat_char_i 1)))) (throw (ex-info "return" {:v repeat_char_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn to_binary [to_binary_n]
  (binding [to_binary_res nil to_binary_v nil] (try (do (when (= to_binary_n 0) (throw (ex-info "return" {:v "0"}))) (set! to_binary_res "") (set! to_binary_v to_binary_n) (while (> to_binary_v 0) (do (set! to_binary_res (str (str (mod to_binary_v 2)) to_binary_res)) (set! to_binary_v (quot to_binary_v 2)))) (throw (ex-info "return" {:v to_binary_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow2 [pow2_exp]
  (binding [pow2_i nil pow2_res nil] (try (do (set! pow2_res 1) (set! pow2_i 0) (while (< pow2_i pow2_exp) (do (set! pow2_res (* pow2_res 2)) (set! pow2_i (+ pow2_i 1)))) (throw (ex-info "return" {:v pow2_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn twos_complement [twos_complement_number]
  (binding [twos_complement_abs_number nil twos_complement_binary_number_length nil twos_complement_complement_binary nil twos_complement_complement_value nil twos_complement_padding nil twos_complement_twos_complement_number nil] (try (do (when (> twos_complement_number 0) (throw (Exception. "input must be a negative integer"))) (when (= twos_complement_number 0) (throw (ex-info "return" {:v "0b0"}))) (set! twos_complement_abs_number (if (< twos_complement_number 0) (- twos_complement_number) twos_complement_number)) (set! twos_complement_binary_number_length (count (to_binary twos_complement_abs_number))) (set! twos_complement_complement_value (- (pow2 twos_complement_binary_number_length) twos_complement_abs_number)) (set! twos_complement_complement_binary (to_binary twos_complement_complement_value)) (set! twos_complement_padding (repeat_char "0" (- twos_complement_binary_number_length (count twos_complement_complement_binary)))) (set! twos_complement_twos_complement_number (str (str "1" twos_complement_padding) twos_complement_complement_binary)) (throw (ex-info "return" {:v (str "0b" twos_complement_twos_complement_number)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (twos_complement 0))
      (println (twos_complement (- 1)))
      (println (twos_complement (- 5)))
      (println (twos_complement (- 17)))
      (println (twos_complement (- 207)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
