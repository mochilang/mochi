(ns main (:refer-clojure :exclude [remove_digit test_remove_digit main]))

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

(declare remove_digit test_remove_digit main)

(def ^:dynamic remove_digit_candidate nil)

(def ^:dynamic remove_digit_divisor nil)

(def ^:dynamic remove_digit_higher nil)

(def ^:dynamic remove_digit_lower nil)

(def ^:dynamic remove_digit_max_val nil)

(def ^:dynamic remove_digit_n nil)

(defn remove_digit [remove_digit_num]
  (binding [remove_digit_candidate nil remove_digit_divisor nil remove_digit_higher nil remove_digit_lower nil remove_digit_max_val nil remove_digit_n nil] (try (do (set! remove_digit_n remove_digit_num) (when (< remove_digit_n 0) (set! remove_digit_n (- remove_digit_n))) (set! remove_digit_max_val 0) (set! remove_digit_divisor 1) (while (<= remove_digit_divisor remove_digit_n) (do (set! remove_digit_higher (quot remove_digit_n (* remove_digit_divisor 10))) (set! remove_digit_lower (mod remove_digit_n remove_digit_divisor)) (set! remove_digit_candidate (+ (* remove_digit_higher remove_digit_divisor) remove_digit_lower)) (when (> remove_digit_candidate remove_digit_max_val) (set! remove_digit_max_val remove_digit_candidate)) (set! remove_digit_divisor (* remove_digit_divisor 10)))) (throw (ex-info "return" {:v remove_digit_max_val}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_remove_digit []
  (do (when (not= (remove_digit 152) 52) (throw (Exception. "remove_digit(152) failed"))) (when (not= (remove_digit 6385) 685) (throw (Exception. "remove_digit(6385) failed"))) (when (not= (remove_digit (- 11)) 1) (throw (Exception. "remove_digit(-11) failed"))) (when (not= (remove_digit 2222222) 222222) (throw (Exception. "remove_digit(2222222) failed")))))

(defn main []
  (do (test_remove_digit) (println (remove_digit 152))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (main)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
