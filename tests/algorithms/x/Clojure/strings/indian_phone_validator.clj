(ns main (:refer-clojure :exclude [all_digits indian_phone_validator]))

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

(declare all_digits indian_phone_validator)

(def ^:dynamic all_digits_c nil)

(def ^:dynamic all_digits_i nil)

(def ^:dynamic first_v nil)

(def ^:dynamic indian_phone_validator_c nil)

(def ^:dynamic indian_phone_validator_s nil)

(defn all_digits [all_digits_s]
  (binding [all_digits_c nil all_digits_i nil] (try (do (when (= (count all_digits_s) 0) (throw (ex-info "return" {:v false}))) (set! all_digits_i 0) (while (< all_digits_i (count all_digits_s)) (do (set! all_digits_c (subs all_digits_s all_digits_i (+ all_digits_i 1))) (when (or (< (compare all_digits_c "0") 0) (> (compare all_digits_c "9") 0)) (throw (ex-info "return" {:v false}))) (set! all_digits_i (+ all_digits_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn indian_phone_validator [indian_phone_validator_phone]
  (binding [first_v nil indian_phone_validator_c nil indian_phone_validator_s nil] (try (do (set! indian_phone_validator_s indian_phone_validator_phone) (when (and (>= (count indian_phone_validator_s) 3) (= (subs indian_phone_validator_s 0 (min 3 (count indian_phone_validator_s))) "+91")) (do (set! indian_phone_validator_s (subs indian_phone_validator_s 3 (min (count indian_phone_validator_s) (count indian_phone_validator_s)))) (when (> (count indian_phone_validator_s) 0) (do (set! indian_phone_validator_c (subs indian_phone_validator_s 0 (+ 0 1))) (when (or (= indian_phone_validator_c "-") (= indian_phone_validator_c " ")) (set! indian_phone_validator_s (subs indian_phone_validator_s 1 (min (count indian_phone_validator_s) (count indian_phone_validator_s))))))))) (when (and (> (count indian_phone_validator_s) 0) (= (subs indian_phone_validator_s 0 (+ 0 1)) "0")) (set! indian_phone_validator_s (subs indian_phone_validator_s 1 (min (count indian_phone_validator_s) (count indian_phone_validator_s))))) (when (and (>= (count indian_phone_validator_s) 2) (= (subs indian_phone_validator_s 0 (min 2 (count indian_phone_validator_s))) "91")) (set! indian_phone_validator_s (subs indian_phone_validator_s 2 (min (count indian_phone_validator_s) (count indian_phone_validator_s))))) (when (not= (count indian_phone_validator_s) 10) (throw (ex-info "return" {:v false}))) (set! first_v (subs indian_phone_validator_s 0 (+ 0 1))) (when (not (or (or (= first_v "7") (= first_v "8")) (= first_v "9"))) (throw (ex-info "return" {:v false}))) (if (not (all_digits indian_phone_validator_s)) false true)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (indian_phone_validator "+91123456789")))
      (println (str (indian_phone_validator "+919876543210")))
      (println (str (indian_phone_validator "01234567896")))
      (println (str (indian_phone_validator "919876543218")))
      (println (str (indian_phone_validator "+91-1234567899")))
      (println (str (indian_phone_validator "+91-9876543218")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
