(ns main (:refer-clojure :exclude [search_all]))

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

(declare search_all)

(def ^:dynamic search_all_i nil)

(def ^:dynamic search_all_m nil)

(def ^:dynamic search_all_positions nil)

(def ^:dynamic search_all_result nil)

(defn search_all [search_all_text search_all_keywords]
  (binding [search_all_i nil search_all_m nil search_all_positions nil search_all_result nil] (try (do (set! search_all_result {}) (doseq [word search_all_keywords] (do (set! search_all_positions []) (set! search_all_m (count word)) (set! search_all_i 0) (while (<= search_all_i (- (count search_all_text) search_all_m)) (do (when (= (subs search_all_text search_all_i (min (+ search_all_i search_all_m) (count search_all_text))) word) (set! search_all_positions (conj search_all_positions search_all_i))) (set! search_all_i (+ search_all_i 1)))) (when (> (count search_all_positions) 0) (set! search_all_result (assoc search_all_result word search_all_positions))))) (throw (ex-info "return" {:v search_all_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_text "whatever, err ... , wherever")

(def ^:dynamic main_keywords ["what" "hat" "ver" "er"])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (search_all main_text main_keywords))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
