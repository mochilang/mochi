(ns main (:refer-clojure :exclude [bit_xor find_unique_number]))

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

(declare bit_xor find_unique_number)

(def ^:dynamic bit_xor_abit nil)

(def ^:dynamic bit_xor_bbit nil)

(def ^:dynamic bit_xor_bit nil)

(def ^:dynamic bit_xor_res nil)

(def ^:dynamic bit_xor_ua nil)

(def ^:dynamic bit_xor_ub nil)

(def ^:dynamic find_unique_number_result nil)

(defn bit_xor [bit_xor_a bit_xor_b]
  (binding [bit_xor_abit nil bit_xor_bbit nil bit_xor_bit nil bit_xor_res nil bit_xor_ua nil bit_xor_ub nil] (try (do (set! bit_xor_ua bit_xor_a) (set! bit_xor_ub bit_xor_b) (set! bit_xor_res 0) (set! bit_xor_bit 1) (while (or (> bit_xor_ua 0) (> bit_xor_ub 0)) (do (set! bit_xor_abit (mod bit_xor_ua 2)) (set! bit_xor_bbit (mod bit_xor_ub 2)) (when (or (and (= bit_xor_abit 1) (= bit_xor_bbit 0)) (and (= bit_xor_abit 0) (= bit_xor_bbit 1))) (set! bit_xor_res (+ bit_xor_res bit_xor_bit))) (set! bit_xor_ua (long (quot bit_xor_ua 2))) (set! bit_xor_ub (long (quot bit_xor_ub 2))) (set! bit_xor_bit (* bit_xor_bit 2)))) (throw (ex-info "return" {:v bit_xor_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_unique_number [find_unique_number_arr]
  (binding [find_unique_number_result nil] (try (do (when (= (count find_unique_number_arr) 0) (throw (Exception. "input list must not be empty"))) (set! find_unique_number_result 0) (doseq [num find_unique_number_arr] (set! find_unique_number_result (bit_xor find_unique_number_result num))) (throw (ex-info "return" {:v find_unique_number_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (find_unique_number [1 1 2 2 3])))
      (println (str (find_unique_number [4 5 4 6 6])))
      (println (str (find_unique_number [7])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
