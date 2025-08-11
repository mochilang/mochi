(ns main (:refer-clojure :exclude [ord lshift rshift is_contains_unique_chars]))

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

(declare ord lshift rshift is_contains_unique_chars)

(def ^:dynamic is_contains_unique_chars_bitmap nil)

(def ^:dynamic is_contains_unique_chars_code nil)

(def ^:dynamic is_contains_unique_chars_i nil)

(def ^:dynamic lshift_i nil)

(def ^:dynamic lshift_result nil)

(def ^:dynamic ord_digits nil)

(def ^:dynamic ord_i nil)

(def ^:dynamic ord_lower nil)

(def ^:dynamic ord_upper nil)

(def ^:dynamic rshift_i nil)

(def ^:dynamic rshift_result nil)

(defn ord [ord_ch]
  (binding [ord_digits nil ord_i nil ord_lower nil ord_upper nil] (try (do (set! ord_lower "abcdefghijklmnopqrstuvwxyz") (set! ord_upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! ord_digits "0123456789") (set! ord_i 0) (while (< ord_i (count ord_lower)) (do (when (= (subs ord_lower ord_i (+ ord_i 1)) ord_ch) (throw (ex-info "return" {:v (+ 97 ord_i)}))) (set! ord_i (+ ord_i 1)))) (set! ord_i 0) (while (< ord_i (count ord_upper)) (do (when (= (subs ord_upper ord_i (+ ord_i 1)) ord_ch) (throw (ex-info "return" {:v (+ 65 ord_i)}))) (set! ord_i (+ ord_i 1)))) (set! ord_i 0) (while (< ord_i (count ord_digits)) (do (when (= (subs ord_digits ord_i (+ ord_i 1)) ord_ch) (throw (ex-info "return" {:v (+ 48 ord_i)}))) (set! ord_i (+ ord_i 1)))) (when (= ord_ch " ") (throw (ex-info "return" {:v 32}))) (when (= ord_ch "_") (throw (ex-info "return" {:v 95}))) (when (= ord_ch ".") (throw (ex-info "return" {:v 46}))) (if (= ord_ch "'") 39 0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn lshift [lshift_num lshift_k]
  (binding [lshift_i nil lshift_result nil] (try (do (set! lshift_result lshift_num) (set! lshift_i 0) (while (< lshift_i lshift_k) (do (set! lshift_result (* lshift_result 2)) (set! lshift_i (+ lshift_i 1)))) (throw (ex-info "return" {:v lshift_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rshift [rshift_num rshift_k]
  (binding [rshift_i nil rshift_result nil] (try (do (set! rshift_result rshift_num) (set! rshift_i 0) (while (< rshift_i rshift_k) (do (set! rshift_result (/ (- rshift_result (mod rshift_result 2)) 2)) (set! rshift_i (+ rshift_i 1)))) (throw (ex-info "return" {:v rshift_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_contains_unique_chars [is_contains_unique_chars_input_str]
  (binding [is_contains_unique_chars_bitmap nil is_contains_unique_chars_code nil is_contains_unique_chars_i nil] (try (do (set! is_contains_unique_chars_bitmap 0) (set! is_contains_unique_chars_i 0) (while (< is_contains_unique_chars_i (count is_contains_unique_chars_input_str)) (do (set! is_contains_unique_chars_code (ord (subs is_contains_unique_chars_input_str is_contains_unique_chars_i (+ is_contains_unique_chars_i 1)))) (when (= (mod (rshift is_contains_unique_chars_bitmap is_contains_unique_chars_code) 2) 1) (throw (ex-info "return" {:v false}))) (set! is_contains_unique_chars_bitmap (+ is_contains_unique_chars_bitmap (lshift 1 is_contains_unique_chars_code))) (set! is_contains_unique_chars_i (+ is_contains_unique_chars_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (is_contains_unique_chars "I_love.py")))
      (println (str (is_contains_unique_chars "I don't love Python")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
