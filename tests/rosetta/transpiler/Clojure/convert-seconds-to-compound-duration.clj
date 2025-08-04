(ns main (:refer-clojure :exclude [timeStr]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare timeStr)

(def ^:dynamic timeStr_comma nil)

(def ^:dynamic timeStr_ds nil)

(def ^:dynamic timeStr_hrs nil)

(def ^:dynamic timeStr_mins nil)

(def ^:dynamic timeStr_res nil)

(def ^:dynamic timeStr_sec nil)

(def ^:dynamic timeStr_wks nil)

(defn timeStr [timeStr_sec_p]
  (binding [timeStr_comma nil timeStr_ds nil timeStr_hrs nil timeStr_mins nil timeStr_res nil timeStr_sec nil timeStr_wks nil] (try (do (set! timeStr_sec timeStr_sec_p) (set! timeStr_wks (quot timeStr_sec 604800)) (set! timeStr_sec (mod timeStr_sec 604800)) (set! timeStr_ds (quot timeStr_sec 86400)) (set! timeStr_sec (mod timeStr_sec 86400)) (set! timeStr_hrs (quot timeStr_sec 3600)) (set! timeStr_sec (mod timeStr_sec 3600)) (set! timeStr_mins (quot timeStr_sec 60)) (set! timeStr_sec (mod timeStr_sec 60)) (set! timeStr_res "") (set! timeStr_comma false) (when (not= timeStr_wks 0) (do (set! timeStr_res (str (str timeStr_res (str timeStr_wks)) " wk")) (set! timeStr_comma true))) (when (not= timeStr_ds 0) (do (when timeStr_comma (set! timeStr_res (str timeStr_res ", "))) (set! timeStr_res (str (str timeStr_res (str timeStr_ds)) " d")) (set! timeStr_comma true))) (when (not= timeStr_hrs 0) (do (when timeStr_comma (set! timeStr_res (str timeStr_res ", "))) (set! timeStr_res (str (str timeStr_res (str timeStr_hrs)) " hr")) (set! timeStr_comma true))) (when (not= timeStr_mins 0) (do (when timeStr_comma (set! timeStr_res (str timeStr_res ", "))) (set! timeStr_res (str (str timeStr_res (str timeStr_mins)) " min")) (set! timeStr_comma true))) (when (not= timeStr_sec 0) (do (when timeStr_comma (set! timeStr_res (str timeStr_res ", "))) (set! timeStr_res (str (str timeStr_res (str timeStr_sec)) " sec")))) (throw (ex-info "return" {:v timeStr_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (timeStr 7259))
      (println (timeStr 86400))
      (println (timeStr 6000000))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
