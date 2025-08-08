(ns main (:refer-clojure :exclude [is_luhn]))

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

(declare is_luhn)

(def ^:dynamic is_luhn_check_digit nil)

(def ^:dynamic is_luhn_digit nil)

(def ^:dynamic is_luhn_doubled nil)

(def ^:dynamic is_luhn_even nil)

(def ^:dynamic is_luhn_i nil)

(def ^:dynamic is_luhn_n nil)

(defn is_luhn [is_luhn_s]
  (binding [is_luhn_check_digit nil is_luhn_digit nil is_luhn_doubled nil is_luhn_even nil is_luhn_i nil is_luhn_n nil] (try (do (set! is_luhn_n (count is_luhn_s)) (when (<= is_luhn_n 1) (throw (ex-info "return" {:v false}))) (set! is_luhn_check_digit (Integer/parseInt (subs is_luhn_s (- is_luhn_n 1) (min is_luhn_n (count is_luhn_s))))) (set! is_luhn_i (- is_luhn_n 2)) (set! is_luhn_even true) (while (>= is_luhn_i 0) (do (set! is_luhn_digit (Integer/parseInt (subs is_luhn_s is_luhn_i (min (+ is_luhn_i 1) (count is_luhn_s))))) (if is_luhn_even (do (set! is_luhn_doubled (* is_luhn_digit 2)) (when (> is_luhn_doubled 9) (set! is_luhn_doubled (- is_luhn_doubled 9))) (set! is_luhn_check_digit (+ is_luhn_check_digit is_luhn_doubled))) (set! is_luhn_check_digit (+ is_luhn_check_digit is_luhn_digit))) (set! is_luhn_even (not is_luhn_even)) (set! is_luhn_i (- is_luhn_i 1)))) (throw (ex-info "return" {:v (= (mod is_luhn_check_digit 10) 0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println ((fn json_str [x] (cond (map? x) (str "{" (clojure.string/join "," (map (fn [[k v]] (str "\"" (name k) "\":" (json_str v))) x)) "}") (sequential? x) (str "[" (clojure.string/join "," (map json_str x)) "]") (string? x) (pr-str x) :else (str x))) (is_luhn "79927398713")))
      (println ((fn json_str [x] (cond (map? x) (str "{" (clojure.string/join "," (map (fn [[k v]] (str "\"" (name k) "\":" (json_str v))) x)) "}") (sequential? x) (str "[" (clojure.string/join "," (map json_str x)) "]") (string? x) (pr-str x) :else (str x))) (is_luhn "79927398714")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
