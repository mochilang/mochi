(ns main (:refer-clojure :exclude [alternative_string_arrange]))

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

(declare alternative_string_arrange)

(def ^:dynamic alternative_string_arrange_i nil)

(def ^:dynamic alternative_string_arrange_len1 nil)

(def ^:dynamic alternative_string_arrange_len2 nil)

(def ^:dynamic alternative_string_arrange_res nil)

(defn alternative_string_arrange [alternative_string_arrange_first_str alternative_string_arrange_second_str]
  (binding [alternative_string_arrange_i nil alternative_string_arrange_len1 nil alternative_string_arrange_len2 nil alternative_string_arrange_res nil] (try (do (set! alternative_string_arrange_len1 (count alternative_string_arrange_first_str)) (set! alternative_string_arrange_len2 (count alternative_string_arrange_second_str)) (set! alternative_string_arrange_res "") (set! alternative_string_arrange_i 0) (while (or (< alternative_string_arrange_i alternative_string_arrange_len1) (< alternative_string_arrange_i alternative_string_arrange_len2)) (do (when (< alternative_string_arrange_i alternative_string_arrange_len1) (set! alternative_string_arrange_res (str alternative_string_arrange_res (subs alternative_string_arrange_first_str alternative_string_arrange_i (+ alternative_string_arrange_i 1))))) (when (< alternative_string_arrange_i alternative_string_arrange_len2) (set! alternative_string_arrange_res (str alternative_string_arrange_res (subs alternative_string_arrange_second_str alternative_string_arrange_i (+ alternative_string_arrange_i 1))))) (set! alternative_string_arrange_i (+ alternative_string_arrange_i 1)))) (throw (ex-info "return" {:v alternative_string_arrange_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (alternative_string_arrange "ABCD" "XY"))
      (println (alternative_string_arrange "XY" "ABCD"))
      (println (alternative_string_arrange "AB" "XYZ"))
      (println (alternative_string_arrange "ABC" ""))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
