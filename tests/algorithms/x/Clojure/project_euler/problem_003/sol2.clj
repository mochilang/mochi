(ns main (:refer-clojure :exclude [largest_prime_factor main]))

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

(declare largest_prime_factor main)

(def ^:dynamic largest_prime_factor_i nil)

(def ^:dynamic largest_prime_factor_num nil)

(def ^:dynamic largest_prime_factor_prime nil)

(defn largest_prime_factor [largest_prime_factor_n]
  (binding [largest_prime_factor_i nil largest_prime_factor_num nil largest_prime_factor_prime nil] (try (do (when (<= largest_prime_factor_n 0) (throw (Exception. "Parameter n must be greater than or equal to one."))) (set! largest_prime_factor_num largest_prime_factor_n) (set! largest_prime_factor_prime 1) (set! largest_prime_factor_i 2) (while (<= (* largest_prime_factor_i largest_prime_factor_i) largest_prime_factor_num) (do (while (= (mod largest_prime_factor_num largest_prime_factor_i) 0) (do (set! largest_prime_factor_prime largest_prime_factor_i) (set! largest_prime_factor_num (/ largest_prime_factor_num largest_prime_factor_i)))) (set! largest_prime_factor_i (+ largest_prime_factor_i 1)))) (when (> largest_prime_factor_num 1) (set! largest_prime_factor_prime largest_prime_factor_num)) (throw (ex-info "return" {:v largest_prime_factor_prime}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (println (str (largest_prime_factor 600851475143))))

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
