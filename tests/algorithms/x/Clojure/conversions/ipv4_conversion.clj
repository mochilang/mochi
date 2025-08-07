(ns main (:refer-clojure :exclude [split_by_dot parse_decimal to_hex2 ipv4_to_decimal alt_ipv4_to_decimal decimal_to_ipv4]))

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

(declare split_by_dot parse_decimal to_hex2 ipv4_to_decimal alt_ipv4_to_decimal decimal_to_ipv4)

(def ^:dynamic alt_ipv4_to_decimal_c nil)

(def ^:dynamic alt_ipv4_to_decimal_digit nil)

(def ^:dynamic alt_ipv4_to_decimal_hex_str nil)

(def ^:dynamic alt_ipv4_to_decimal_i nil)

(def ^:dynamic alt_ipv4_to_decimal_j nil)

(def ^:dynamic alt_ipv4_to_decimal_k nil)

(def ^:dynamic alt_ipv4_to_decimal_oct nil)

(def ^:dynamic alt_ipv4_to_decimal_parts nil)

(def ^:dynamic alt_ipv4_to_decimal_value nil)

(def ^:dynamic decimal_to_ipv4_i nil)

(def ^:dynamic decimal_to_ipv4_j nil)

(def ^:dynamic decimal_to_ipv4_n nil)

(def ^:dynamic decimal_to_ipv4_octet nil)

(def ^:dynamic decimal_to_ipv4_parts nil)

(def ^:dynamic decimal_to_ipv4_res nil)

(def ^:dynamic ipv4_to_decimal_i nil)

(def ^:dynamic ipv4_to_decimal_oct nil)

(def ^:dynamic ipv4_to_decimal_parts nil)

(def ^:dynamic ipv4_to_decimal_result nil)

(def ^:dynamic parse_decimal_c nil)

(def ^:dynamic parse_decimal_i nil)

(def ^:dynamic parse_decimal_value nil)

(def ^:dynamic split_by_dot_c nil)

(def ^:dynamic split_by_dot_current nil)

(def ^:dynamic split_by_dot_i nil)

(def ^:dynamic split_by_dot_res nil)

(def ^:dynamic to_hex2_d nil)

(def ^:dynamic to_hex2_res nil)

(def ^:dynamic to_hex2_x nil)

(def ^:dynamic main_hex_digits "0123456789abcdef")

(defn split_by_dot [split_by_dot_s]
  (binding [split_by_dot_c nil split_by_dot_current nil split_by_dot_i nil split_by_dot_res nil] (try (do (set! split_by_dot_res []) (set! split_by_dot_current "") (set! split_by_dot_i 0) (while (< split_by_dot_i (count split_by_dot_s)) (do (set! split_by_dot_c (nth split_by_dot_s split_by_dot_i)) (if (= split_by_dot_c ".") (do (set! split_by_dot_res (conj split_by_dot_res split_by_dot_current)) (set! split_by_dot_current "")) (set! split_by_dot_current (str split_by_dot_current split_by_dot_c))) (set! split_by_dot_i (+ split_by_dot_i 1)))) (set! split_by_dot_res (conj split_by_dot_res split_by_dot_current)) (throw (ex-info "return" {:v split_by_dot_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn parse_decimal [parse_decimal_s]
  (binding [parse_decimal_c nil parse_decimal_i nil parse_decimal_value nil] (try (do (when (= (count parse_decimal_s) 0) (throw (Exception. "Invalid IPv4 address format"))) (set! parse_decimal_value 0) (set! parse_decimal_i 0) (while (< parse_decimal_i (count parse_decimal_s)) (do (set! parse_decimal_c (nth parse_decimal_s parse_decimal_i)) (when (or (< (compare parse_decimal_c "0") 0) (> (compare parse_decimal_c "9") 0)) (throw (Exception. "Invalid IPv4 address format"))) (set! parse_decimal_value (+ (* parse_decimal_value 10) (long parse_decimal_c))) (set! parse_decimal_i (+ parse_decimal_i 1)))) (throw (ex-info "return" {:v parse_decimal_value}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn to_hex2 [to_hex2_n]
  (binding [to_hex2_d nil to_hex2_res nil to_hex2_x nil] (try (do (set! to_hex2_x to_hex2_n) (set! to_hex2_res "") (while (> to_hex2_x 0) (do (set! to_hex2_d (mod to_hex2_x 16)) (set! to_hex2_res (str (nth main_hex_digits to_hex2_d) to_hex2_res)) (set! to_hex2_x (quot to_hex2_x 16)))) (while (< (count to_hex2_res) 2) (set! to_hex2_res (str "0" to_hex2_res))) (throw (ex-info "return" {:v to_hex2_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn ipv4_to_decimal [ipv4_to_decimal_ipv4_address]
  (binding [ipv4_to_decimal_i nil ipv4_to_decimal_oct nil ipv4_to_decimal_parts nil ipv4_to_decimal_result nil] (try (do (set! ipv4_to_decimal_parts (split_by_dot ipv4_to_decimal_ipv4_address)) (when (not= (count ipv4_to_decimal_parts) 4) (throw (Exception. "Invalid IPv4 address format"))) (set! ipv4_to_decimal_result 0) (set! ipv4_to_decimal_i 0) (while (< ipv4_to_decimal_i 4) (do (set! ipv4_to_decimal_oct (parse_decimal (nth ipv4_to_decimal_parts ipv4_to_decimal_i))) (when (or (< ipv4_to_decimal_oct 0) (> ipv4_to_decimal_oct 255)) (throw (Exception. (str "Invalid IPv4 octet " (str ipv4_to_decimal_oct))))) (set! ipv4_to_decimal_result (+ (* ipv4_to_decimal_result 256) ipv4_to_decimal_oct)) (set! ipv4_to_decimal_i (+ ipv4_to_decimal_i 1)))) (throw (ex-info "return" {:v ipv4_to_decimal_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn alt_ipv4_to_decimal [alt_ipv4_to_decimal_ipv4_address]
  (binding [alt_ipv4_to_decimal_c nil alt_ipv4_to_decimal_digit nil alt_ipv4_to_decimal_hex_str nil alt_ipv4_to_decimal_i nil alt_ipv4_to_decimal_j nil alt_ipv4_to_decimal_k nil alt_ipv4_to_decimal_oct nil alt_ipv4_to_decimal_parts nil alt_ipv4_to_decimal_value nil] (try (do (set! alt_ipv4_to_decimal_parts (split_by_dot alt_ipv4_to_decimal_ipv4_address)) (when (not= (count alt_ipv4_to_decimal_parts) 4) (throw (Exception. "Invalid IPv4 address format"))) (set! alt_ipv4_to_decimal_hex_str "") (set! alt_ipv4_to_decimal_i 0) (while (< alt_ipv4_to_decimal_i 4) (do (set! alt_ipv4_to_decimal_oct (parse_decimal (nth alt_ipv4_to_decimal_parts alt_ipv4_to_decimal_i))) (when (or (< alt_ipv4_to_decimal_oct 0) (> alt_ipv4_to_decimal_oct 255)) (throw (Exception. (str "Invalid IPv4 octet " (str alt_ipv4_to_decimal_oct))))) (set! alt_ipv4_to_decimal_hex_str (str alt_ipv4_to_decimal_hex_str (to_hex2 alt_ipv4_to_decimal_oct))) (set! alt_ipv4_to_decimal_i (+ alt_ipv4_to_decimal_i 1)))) (set! alt_ipv4_to_decimal_value 0) (set! alt_ipv4_to_decimal_k 0) (while (< alt_ipv4_to_decimal_k (count alt_ipv4_to_decimal_hex_str)) (do (set! alt_ipv4_to_decimal_c (nth alt_ipv4_to_decimal_hex_str alt_ipv4_to_decimal_k)) (set! alt_ipv4_to_decimal_digit (- 0 1)) (set! alt_ipv4_to_decimal_j 0) (while (< alt_ipv4_to_decimal_j (count main_hex_digits)) (do (when (= (nth main_hex_digits alt_ipv4_to_decimal_j) alt_ipv4_to_decimal_c) (set! alt_ipv4_to_decimal_digit alt_ipv4_to_decimal_j)) (set! alt_ipv4_to_decimal_j (+ alt_ipv4_to_decimal_j 1)))) (when (< alt_ipv4_to_decimal_digit 0) (throw (Exception. "Invalid hex digit"))) (set! alt_ipv4_to_decimal_value (+ (* alt_ipv4_to_decimal_value 16) alt_ipv4_to_decimal_digit)) (set! alt_ipv4_to_decimal_k (+ alt_ipv4_to_decimal_k 1)))) (throw (ex-info "return" {:v alt_ipv4_to_decimal_value}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decimal_to_ipv4 [decimal_to_ipv4_decimal_ipv4]
  (binding [decimal_to_ipv4_i nil decimal_to_ipv4_j nil decimal_to_ipv4_n nil decimal_to_ipv4_octet nil decimal_to_ipv4_parts nil decimal_to_ipv4_res nil] (try (do (when (or (< decimal_to_ipv4_decimal_ipv4 0) (> decimal_to_ipv4_decimal_ipv4 4294967295)) (throw (Exception. "Invalid decimal IPv4 address"))) (set! decimal_to_ipv4_n decimal_to_ipv4_decimal_ipv4) (set! decimal_to_ipv4_parts []) (set! decimal_to_ipv4_i 0) (while (< decimal_to_ipv4_i 4) (do (set! decimal_to_ipv4_octet (mod decimal_to_ipv4_n 256)) (set! decimal_to_ipv4_parts (conj decimal_to_ipv4_parts (str decimal_to_ipv4_octet))) (set! decimal_to_ipv4_n (quot decimal_to_ipv4_n 256)) (set! decimal_to_ipv4_i (+ decimal_to_ipv4_i 1)))) (set! decimal_to_ipv4_res "") (set! decimal_to_ipv4_j (- (count decimal_to_ipv4_parts) 1)) (while (>= decimal_to_ipv4_j 0) (do (set! decimal_to_ipv4_res (str decimal_to_ipv4_res (nth decimal_to_ipv4_parts decimal_to_ipv4_j))) (when (> decimal_to_ipv4_j 0) (set! decimal_to_ipv4_res (str decimal_to_ipv4_res "."))) (set! decimal_to_ipv4_j (- decimal_to_ipv4_j 1)))) (throw (ex-info "return" {:v decimal_to_ipv4_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (ipv4_to_decimal "192.168.0.1"))
      (println (ipv4_to_decimal "10.0.0.255"))
      (println (alt_ipv4_to_decimal "192.168.0.1"))
      (println (alt_ipv4_to_decimal "10.0.0.255"))
      (println (decimal_to_ipv4 3232235521))
      (println (decimal_to_ipv4 167772415))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
