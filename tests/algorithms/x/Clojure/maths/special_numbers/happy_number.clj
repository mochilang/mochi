(ns main (:refer-clojure :exclude [is_happy_number test_is_happy_number main]))

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

(declare is_happy_number test_is_happy_number main)

(def ^:dynamic is_happy_number_digit nil)

(def ^:dynamic is_happy_number_i nil)

(def ^:dynamic is_happy_number_n nil)

(def ^:dynamic is_happy_number_seen nil)

(def ^:dynamic is_happy_number_temp nil)

(def ^:dynamic is_happy_number_total nil)

(defn is_happy_number [is_happy_number_num]
  (binding [is_happy_number_digit nil is_happy_number_i nil is_happy_number_n nil is_happy_number_seen nil is_happy_number_temp nil is_happy_number_total nil] (try (do (when (<= is_happy_number_num 0) (throw (Exception. "num must be a positive integer"))) (set! is_happy_number_seen []) (set! is_happy_number_n is_happy_number_num) (while (not= is_happy_number_n 1) (do (set! is_happy_number_i 0) (while (< is_happy_number_i (count is_happy_number_seen)) (do (when (= (nth is_happy_number_seen is_happy_number_i) is_happy_number_n) (throw (ex-info "return" {:v false}))) (set! is_happy_number_i (+ is_happy_number_i 1)))) (set! is_happy_number_seen (conj is_happy_number_seen is_happy_number_n)) (set! is_happy_number_total 0) (set! is_happy_number_temp is_happy_number_n) (while (> is_happy_number_temp 0) (do (set! is_happy_number_digit (mod is_happy_number_temp 10)) (set! is_happy_number_total (+ is_happy_number_total (* is_happy_number_digit is_happy_number_digit))) (set! is_happy_number_temp (quot is_happy_number_temp 10)))) (set! is_happy_number_n is_happy_number_total))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_is_happy_number []
  (do (when (not (is_happy_number 19)) (throw (Exception. "19 should be happy"))) (when (is_happy_number 2) (throw (Exception. "2 should be unhappy"))) (when (not (is_happy_number 23)) (throw (Exception. "23 should be happy"))) (when (not (is_happy_number 1)) (throw (Exception. "1 should be happy")))))

(defn main []
  (do (test_is_happy_number) (println (is_happy_number 19))))

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
