(ns main (:refer-clojure :exclude [split_by_dot is_digit_str parse_decimal is_ip_v4_address_valid]))

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

(declare split_by_dot is_digit_str parse_decimal is_ip_v4_address_valid)

(def ^:dynamic is_digit_str_c nil)

(def ^:dynamic is_digit_str_i nil)

(def ^:dynamic is_ip_v4_address_valid_i nil)

(def ^:dynamic is_ip_v4_address_valid_number nil)

(def ^:dynamic is_ip_v4_address_valid_oct nil)

(def ^:dynamic is_ip_v4_address_valid_octets nil)

(def ^:dynamic parse_decimal_c nil)

(def ^:dynamic parse_decimal_i nil)

(def ^:dynamic parse_decimal_value nil)

(def ^:dynamic split_by_dot_c nil)

(def ^:dynamic split_by_dot_current nil)

(def ^:dynamic split_by_dot_i nil)

(def ^:dynamic split_by_dot_res nil)

(defn split_by_dot [split_by_dot_s]
  (binding [split_by_dot_c nil split_by_dot_current nil split_by_dot_i nil split_by_dot_res nil] (try (do (set! split_by_dot_res []) (set! split_by_dot_current "") (set! split_by_dot_i 0) (while (< split_by_dot_i (count split_by_dot_s)) (do (set! split_by_dot_c (subs split_by_dot_s split_by_dot_i (+ split_by_dot_i 1))) (if (= split_by_dot_c ".") (do (set! split_by_dot_res (conj split_by_dot_res split_by_dot_current)) (set! split_by_dot_current "")) (set! split_by_dot_current (str split_by_dot_current split_by_dot_c))) (set! split_by_dot_i (+ split_by_dot_i 1)))) (set! split_by_dot_res (conj split_by_dot_res split_by_dot_current)) (throw (ex-info "return" {:v split_by_dot_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_digit_str [is_digit_str_s]
  (binding [is_digit_str_c nil is_digit_str_i nil] (try (do (when (= (count is_digit_str_s) 0) (throw (ex-info "return" {:v false}))) (set! is_digit_str_i 0) (while (< is_digit_str_i (count is_digit_str_s)) (do (set! is_digit_str_c (subs is_digit_str_s is_digit_str_i (+ is_digit_str_i 1))) (when (or (< (compare is_digit_str_c "0") 0) (> (compare is_digit_str_c "9") 0)) (throw (ex-info "return" {:v false}))) (set! is_digit_str_i (+ is_digit_str_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn parse_decimal [parse_decimal_s]
  (binding [parse_decimal_c nil parse_decimal_i nil parse_decimal_value nil] (try (do (set! parse_decimal_value 0) (set! parse_decimal_i 0) (while (< parse_decimal_i (count parse_decimal_s)) (do (set! parse_decimal_c (subs parse_decimal_s parse_decimal_i (+ parse_decimal_i 1))) (set! parse_decimal_value (+ (* parse_decimal_value 10) (Long/parseLong parse_decimal_c))) (set! parse_decimal_i (+ parse_decimal_i 1)))) (throw (ex-info "return" {:v parse_decimal_value}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_ip_v4_address_valid [is_ip_v4_address_valid_ip]
  (binding [is_ip_v4_address_valid_i nil is_ip_v4_address_valid_number nil is_ip_v4_address_valid_oct nil is_ip_v4_address_valid_octets nil] (try (do (set! is_ip_v4_address_valid_octets (split_by_dot is_ip_v4_address_valid_ip)) (when (not= (count is_ip_v4_address_valid_octets) 4) (throw (ex-info "return" {:v false}))) (set! is_ip_v4_address_valid_i 0) (while (< is_ip_v4_address_valid_i 4) (do (set! is_ip_v4_address_valid_oct (nth is_ip_v4_address_valid_octets is_ip_v4_address_valid_i)) (when (not (is_digit_str is_ip_v4_address_valid_oct)) (throw (ex-info "return" {:v false}))) (set! is_ip_v4_address_valid_number (parse_decimal is_ip_v4_address_valid_oct)) (when (not= (count (str is_ip_v4_address_valid_number)) (count is_ip_v4_address_valid_oct)) (throw (ex-info "return" {:v false}))) (when (or (< is_ip_v4_address_valid_number 0) (> is_ip_v4_address_valid_number 255)) (throw (ex-info "return" {:v false}))) (set! is_ip_v4_address_valid_i (+ is_ip_v4_address_valid_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (is_ip_v4_address_valid "192.168.0.23")))
      (println (str (is_ip_v4_address_valid "192.256.15.8")))
      (println (str (is_ip_v4_address_valid "172.100.0.8")))
      (println (str (is_ip_v4_address_valid "255.256.0.256")))
      (println (str (is_ip_v4_address_valid "1.2.33333333.4")))
      (println (str (is_ip_v4_address_valid "1.2.-3.4")))
      (println (str (is_ip_v4_address_valid "1.2.3")))
      (println (str (is_ip_v4_address_valid "1.2.3.4.5")))
      (println (str (is_ip_v4_address_valid "1.2.A.4")))
      (println (str (is_ip_v4_address_valid "0.0.0.0")))
      (println (str (is_ip_v4_address_valid "1.2.3.")))
      (println (str (is_ip_v4_address_valid "1.2.3.05")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
