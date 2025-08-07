(ns main (:refer-clojure :exclude [clamp change_brightness]))

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

(declare clamp change_brightness)

(def ^:dynamic change_brightness_i nil)

(def ^:dynamic change_brightness_j nil)

(def ^:dynamic change_brightness_result nil)

(def ^:dynamic change_brightness_row_res nil)

(defn clamp [clamp_value]
  (try (do (when (< clamp_value 0) (throw (ex-info "return" {:v 0}))) (if (> clamp_value 255) 255 clamp_value)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn change_brightness [change_brightness_img change_brightness_level]
  (binding [change_brightness_i nil change_brightness_j nil change_brightness_result nil change_brightness_row_res nil] (try (do (when (or (< change_brightness_level (- 255)) (> change_brightness_level 255)) (throw (Exception. "level must be between -255 and 255"))) (set! change_brightness_result []) (set! change_brightness_i 0) (while (< change_brightness_i (count change_brightness_img)) (do (set! change_brightness_row_res []) (set! change_brightness_j 0) (while (< change_brightness_j (count (nth change_brightness_img change_brightness_i))) (do (set! change_brightness_row_res (conj change_brightness_row_res (clamp (+ (nth (nth change_brightness_img change_brightness_i) change_brightness_j) change_brightness_level)))) (set! change_brightness_j (+ change_brightness_j 1)))) (set! change_brightness_result (conj change_brightness_result change_brightness_row_res)) (set! change_brightness_i (+ change_brightness_i 1)))) (throw (ex-info "return" {:v change_brightness_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_sample [[100 150] [200 250]])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (change_brightness main_sample 30))
      (println (change_brightness main_sample (- 60)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
