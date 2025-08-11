(ns main (:refer-clojure :exclude [find stock_price]))

(require 'clojure.set)

(defrecord Pages [AAPL AMZN IBM GOOG MSFT ORCL])

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

(def ^:dynamic find_i nil)

(def ^:dynamic find_limit nil)

(def ^:dynamic stock_price_end_idx nil)

(def ^:dynamic stock_price_html nil)

(def ^:dynamic stock_price_marker nil)

(def ^:dynamic stock_price_pages nil)

(def ^:dynamic stock_price_price_start nil)

(def ^:dynamic stock_price_start_idx nil)

(declare find stock_price)

(defn find [find_text find_pattern find_start]
  (binding [find_i nil find_limit nil] (try (do (set! find_i find_start) (set! find_limit (- (count find_text) (count find_pattern))) (while (<= find_i find_limit) (do (when (= (subs find_text find_i (min (+ find_i (count find_pattern)) (count find_text))) find_pattern) (throw (ex-info "return" {:v find_i}))) (set! find_i (+ find_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn stock_price [stock_price_symbol]
  (binding [stock_price_end_idx nil stock_price_html nil stock_price_marker nil stock_price_pages nil stock_price_price_start nil stock_price_start_idx nil] (try (do (set! stock_price_pages {"AAPL" "<span data-testid=\"qsp-price\">228.43</span>" "AMZN" "<span data-testid=\"qsp-price\">201.85</span>" "GOOG" "<span data-testid=\"qsp-price\">177.86</span>" "IBM" "<span data-testid=\"qsp-price\">210.30</span>" "MSFT" "<span data-testid=\"qsp-price\">414.82</span>" "ORCL" "<span data-testid=\"qsp-price\">188.87</span>"}) (when (in stock_price_symbol stock_price_pages) (do (set! stock_price_html (get stock_price_pages stock_price_symbol)) (set! stock_price_marker "<span data-testid=\"qsp-price\">") (set! stock_price_start_idx (find stock_price_html stock_price_marker 0)) (when (not= stock_price_start_idx (- 1)) (do (set! stock_price_price_start (+ stock_price_start_idx (count stock_price_marker))) (set! stock_price_end_idx (find stock_price_html "</span>" stock_price_price_start)) (when (not= stock_price_end_idx (- 1)) (throw (ex-info "return" {:v (subs stock_price_html stock_price_price_start (min stock_price_end_idx (count stock_price_html)))}))))))) (throw (ex-info "return" {:v "No <fin-streamer> tag with the specified data-testid attribute found."}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (doseq [symbol ["AAPL" "AMZN" "IBM" "GOOG" "MSFT" "ORCL"]] (println (str (str (str "Current " symbol) " stock price is ") (stock_price symbol))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
