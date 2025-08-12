(ns main (:refer-clojure :exclude [largest_prime_factor]))

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

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare largest_prime_factor)

(def ^:dynamic largest_prime_factor_ans nil)

(def ^:dynamic largest_prime_factor_i nil)

(def ^:dynamic largest_prime_factor_m nil)

(defn largest_prime_factor [largest_prime_factor_n]
  (binding [largest_prime_factor_ans nil largest_prime_factor_i nil largest_prime_factor_m nil] (try (do (when (<= largest_prime_factor_n 1) (throw (ex-info "return" {:v largest_prime_factor_n}))) (set! largest_prime_factor_i 2) (set! largest_prime_factor_ans 0) (set! largest_prime_factor_m largest_prime_factor_n) (when (= largest_prime_factor_m 2) (throw (ex-info "return" {:v 2}))) (while (> largest_prime_factor_m 2) (do (while (not= (mod largest_prime_factor_m largest_prime_factor_i) 0) (set! largest_prime_factor_i (+ largest_prime_factor_i 1))) (set! largest_prime_factor_ans largest_prime_factor_i) (while (= (mod largest_prime_factor_m largest_prime_factor_i) 0) (set! largest_prime_factor_m (/ largest_prime_factor_m largest_prime_factor_i))) (set! largest_prime_factor_i (+ largest_prime_factor_i 1)))) (throw (ex-info "return" {:v largest_prime_factor_ans}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println ((fn json_str [x] (cond (map? x) (str "{" (clojure.string/join "," (map (fn [[k v]] (str "\"" (name k) "\":" (json_str v))) x)) "}") (sequential? x) (str "[" (clojure.string/join "," (map json_str x)) "]") (string? x) (pr-str x) :else (str x))) (largest_prime_factor 13195)))
      (println ((fn json_str [x] (cond (map? x) (str "{" (clojure.string/join "," (map (fn [[k v]] (str "\"" (name k) "\":" (json_str v))) x)) "}") (sequential? x) (str "[" (clojure.string/join "," (map json_str x)) "]") (string? x) (pr-str x) :else (str x))) (largest_prime_factor 10)))
      (println ((fn json_str [x] (cond (map? x) (str "{" (clojure.string/join "," (map (fn [[k v]] (str "\"" (name k) "\":" (json_str v))) x)) "}") (sequential? x) (str "[" (clojure.string/join "," (map json_str x)) "]") (string? x) (pr-str x) :else (str x))) (largest_prime_factor 17)))
      (println ((fn json_str [x] (cond (map? x) (str "{" (clojure.string/join "," (map (fn [[k v]] (str "\"" (name k) "\":" (json_str v))) x)) "}") (sequential? x) (str "[" (clojure.string/join "," (map json_str x)) "]") (string? x) (pr-str x) :else (str x))) (largest_prime_factor 600851475143)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
