(ns main (:refer-clojure :exclude [excel_title_to_column main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare excel_title_to_column main)

(def ^:dynamic excel_title_to_column_ch nil)

(def ^:dynamic excel_title_to_column_found nil)

(def ^:dynamic excel_title_to_column_i nil)

(def ^:dynamic excel_title_to_column_idx nil)

(def ^:dynamic excel_title_to_column_result nil)

(def ^:dynamic excel_title_to_column_value nil)

(def ^:dynamic main_letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn excel_title_to_column [excel_title_to_column_title]
  (binding [excel_title_to_column_ch nil excel_title_to_column_found nil excel_title_to_column_i nil excel_title_to_column_idx nil excel_title_to_column_result nil excel_title_to_column_value nil] (try (do (set! excel_title_to_column_result 0) (set! excel_title_to_column_i 0) (while (< excel_title_to_column_i (count excel_title_to_column_title)) (do (set! excel_title_to_column_ch (subs excel_title_to_column_title excel_title_to_column_i (min (+ excel_title_to_column_i 1) (count excel_title_to_column_title)))) (set! excel_title_to_column_value 0) (set! excel_title_to_column_idx 0) (set! excel_title_to_column_found false) (loop [while_flag_1 true] (when (and while_flag_1 (< excel_title_to_column_idx (count main_letters))) (cond (= (subs main_letters excel_title_to_column_idx (min (+ excel_title_to_column_idx 1) (count main_letters))) excel_title_to_column_ch) (do (set! excel_title_to_column_value (+ excel_title_to_column_idx 1)) (set! excel_title_to_column_found true) (recur false)) :else (do (set! excel_title_to_column_idx (+ excel_title_to_column_idx 1)) (recur while_flag_1))))) (when (not excel_title_to_column_found) (throw (Exception. "title must contain only uppercase A-Z"))) (set! excel_title_to_column_result (+ (* excel_title_to_column_result 26) excel_title_to_column_value)) (set! excel_title_to_column_i (+ excel_title_to_column_i 1)))) (throw (ex-info "return" {:v excel_title_to_column_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (excel_title_to_column "A")) (println (excel_title_to_column "B")) (println (excel_title_to_column "AB")) (println (excel_title_to_column "Z"))))

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
