(ns main (:refer-clojure :exclude [parse_int is_polish_national_id]))

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

(declare parse_int is_polish_national_id)

(def ^:dynamic is_polish_national_id_checksum nil)

(def ^:dynamic is_polish_national_id_day nil)

(def ^:dynamic is_polish_national_id_digit nil)

(def ^:dynamic is_polish_national_id_i nil)

(def ^:dynamic is_polish_national_id_input_int nil)

(def ^:dynamic is_polish_national_id_month nil)

(def ^:dynamic is_polish_national_id_multipliers nil)

(def ^:dynamic is_polish_national_id_subtotal nil)

(def ^:dynamic parse_int_c nil)

(def ^:dynamic parse_int_i nil)

(def ^:dynamic parse_int_value nil)

(defn parse_int [parse_int_s]
  (binding [parse_int_c nil parse_int_i nil parse_int_value nil] (try (do (set! parse_int_value 0) (set! parse_int_i 0) (while (< parse_int_i (count parse_int_s)) (do (set! parse_int_c (subs parse_int_s parse_int_i (+ parse_int_i 1))) (set! parse_int_value (+ (* parse_int_value 10) (Long/parseLong parse_int_c))) (set! parse_int_i (+ parse_int_i 1)))) (throw (ex-info "return" {:v parse_int_value}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_polish_national_id [is_polish_national_id_id]
  (binding [is_polish_national_id_checksum nil is_polish_national_id_day nil is_polish_national_id_digit nil is_polish_national_id_i nil is_polish_national_id_input_int nil is_polish_national_id_month nil is_polish_national_id_multipliers nil is_polish_national_id_subtotal nil] (try (do (when (= (count is_polish_national_id_id) 0) (throw (ex-info "return" {:v false}))) (when (= (subs is_polish_national_id_id 0 (min 1 (count is_polish_national_id_id))) "-") (throw (ex-info "return" {:v false}))) (set! is_polish_national_id_input_int (parse_int is_polish_national_id_id)) (when (or (< is_polish_national_id_input_int 10100000) (> is_polish_national_id_input_int 99923199999)) (throw (ex-info "return" {:v false}))) (set! is_polish_national_id_month (parse_int (subs is_polish_national_id_id 2 (min 4 (count is_polish_national_id_id))))) (when (not (or (or (or (or (and (>= is_polish_national_id_month 1) (<= is_polish_national_id_month 12)) (and (>= is_polish_national_id_month 21) (<= is_polish_national_id_month 32))) (and (>= is_polish_national_id_month 41) (<= is_polish_national_id_month 52))) (and (>= is_polish_national_id_month 61) (<= is_polish_national_id_month 72))) (and (>= is_polish_national_id_month 81) (<= is_polish_national_id_month 92)))) (throw (ex-info "return" {:v false}))) (set! is_polish_national_id_day (parse_int (subs is_polish_national_id_id 4 (min 6 (count is_polish_national_id_id))))) (when (or (< is_polish_national_id_day 1) (> is_polish_national_id_day 31)) (throw (ex-info "return" {:v false}))) (set! is_polish_national_id_multipliers [1 3 7 9 1 3 7 9 1 3]) (set! is_polish_national_id_subtotal 0) (set! is_polish_national_id_i 0) (while (< is_polish_national_id_i (count is_polish_national_id_multipliers)) (do (set! is_polish_national_id_digit (parse_int (subs is_polish_national_id_id is_polish_national_id_i (min (+ is_polish_national_id_i 1) (count is_polish_national_id_id))))) (set! is_polish_national_id_subtotal (+ is_polish_national_id_subtotal (mod (* is_polish_national_id_digit (nth is_polish_national_id_multipliers is_polish_national_id_i)) 10))) (set! is_polish_national_id_i (+ is_polish_national_id_i 1)))) (set! is_polish_national_id_checksum (- 10 (mod is_polish_national_id_subtotal 10))) (throw (ex-info "return" {:v (= is_polish_national_id_checksum (mod is_polish_national_id_input_int 10))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (is_polish_national_id "02070803628")))
      (println (str (is_polish_national_id "02150803629")))
      (println (str (is_polish_national_id "02075503622")))
      (println (str (is_polish_national_id "-99012212349")))
      (println (str (is_polish_national_id "990122123499999")))
      (println (str (is_polish_national_id "02070803621")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
