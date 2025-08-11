(ns main (:refer-clojure :exclude [rate_of convert_currency]))

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

(declare rate_of convert_currency)

(def ^:dynamic convert_currency_from_rate nil)

(def ^:dynamic convert_currency_to_rate nil)

(def ^:dynamic convert_currency_usd nil)

(def ^:dynamic main_rates [{:code "USD" :rate 1.0} {:code "EUR" :rate 0.9} {:code "INR" :rate 83.0} {:code "JPY" :rate 156.0} {:code "GBP" :rate 0.78}])

(defn rate_of [rate_of_code]
  (try (do (doseq [r main_rates] (when (= (:code r) rate_of_code) (throw (ex-info "return" {:v (:rate r)})))) (throw (ex-info "return" {:v 0.0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn convert_currency [convert_currency_from_ convert_currency_to convert_currency_amount]
  (binding [convert_currency_from_rate nil convert_currency_to_rate nil convert_currency_usd nil] (try (do (set! convert_currency_from_rate (rate_of convert_currency_from_)) (set! convert_currency_to_rate (rate_of convert_currency_to)) (when (or (= convert_currency_from_rate 0.0) (= convert_currency_to_rate 0.0)) (throw (ex-info "return" {:v 0.0}))) (set! convert_currency_usd (/ convert_currency_amount convert_currency_from_rate)) (throw (ex-info "return" {:v (* convert_currency_usd convert_currency_to_rate)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_result (convert_currency "USD" "INR" 10.0))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str main_result))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
