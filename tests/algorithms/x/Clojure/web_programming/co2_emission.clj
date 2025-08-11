(ns main (:refer-clojure :exclude [fetch_last_half_hour fetch_from_to main]))

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

(declare fetch_last_half_hour fetch_from_to main)

(def ^:dynamic fetch_from_to_resp nil)

(def ^:dynamic fetch_from_to_url nil)

(def ^:dynamic fetch_last_half_hour_entry nil)

(def ^:dynamic fetch_last_half_hour_resp nil)

(def ^:dynamic main_e nil)

(def ^:dynamic main_entries nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_last nil)

(def ^:dynamic main_BASE_URL "https://api.carbonintensity.org.uk/intensity")

(defn fetch_last_half_hour []
  (binding [fetch_last_half_hour_entry nil fetch_last_half_hour_resp nil] (try (do (set! fetch_last_half_hour_resp (_fetch main_BASE_URL)) (set! fetch_last_half_hour_entry (get (:data fetch_last_half_hour_resp) 0)) (throw (ex-info "return" {:v (:actual (:intensity fetch_last_half_hour_entry))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn fetch_from_to [fetch_from_to_start fetch_from_to_end]
  (binding [fetch_from_to_resp nil fetch_from_to_url nil] (try (do (set! fetch_from_to_url (str (str (str (str main_BASE_URL "/") fetch_from_to_start) "/") fetch_from_to_end)) (set! fetch_from_to_resp (_fetch fetch_from_to_url)) (throw (ex-info "return" {:v (:data fetch_from_to_resp)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_e nil main_entries nil main_i nil main_last nil] (do (set! main_entries (fetch_from_to "2020-10-01" "2020-10-03")) (set! main_i 0) (while (< main_i (count main_entries)) (do (set! main_e (nth main_entries main_i)) (println "from" (:from main_e) "to" (:to main_e) ":" (:actual (:intensity main_e))) (set! main_i (+ main_i 1)))) (set! main_last (fetch_last_half_hour)) (println "fetch_last_half_hour() =" main_last))))

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
