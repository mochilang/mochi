(ns main (:refer-clojure :exclude [min3 edit_distance main]))

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

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare min3 edit_distance main)

(def ^:dynamic edit_distance_delete_cost nil)

(def ^:dynamic edit_distance_delta nil)

(def ^:dynamic edit_distance_insert_cost nil)

(def ^:dynamic edit_distance_last_source nil)

(def ^:dynamic edit_distance_last_target nil)

(def ^:dynamic edit_distance_replace_cost nil)

(def ^:dynamic main_result nil)

(def ^:dynamic min3_m nil)

(defn min3 [min3_a min3_b min3_c]
  (binding [min3_m nil] (try (do (set! min3_m min3_a) (when (< min3_b min3_m) (set! min3_m min3_b)) (when (< min3_c min3_m) (set! min3_m min3_c)) (throw (ex-info "return" {:v min3_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn edit_distance [edit_distance_source edit_distance_target]
  (binding [edit_distance_delete_cost nil edit_distance_delta nil edit_distance_insert_cost nil edit_distance_last_source nil edit_distance_last_target nil edit_distance_replace_cost nil] (try (do (when (= (count edit_distance_source) 0) (throw (ex-info "return" {:v (count edit_distance_target)}))) (when (= (count edit_distance_target) 0) (throw (ex-info "return" {:v (count edit_distance_source)}))) (set! edit_distance_last_source (subs edit_distance_source (- (count edit_distance_source) 1) (min (count edit_distance_source) (count edit_distance_source)))) (set! edit_distance_last_target (subs edit_distance_target (- (count edit_distance_target) 1) (min (count edit_distance_target) (count edit_distance_target)))) (set! edit_distance_delta (if (= edit_distance_last_source edit_distance_last_target) 0 1)) (set! edit_distance_delete_cost (+ (edit_distance (subs edit_distance_source 0 (min (- (count edit_distance_source) 1) (count edit_distance_source))) edit_distance_target) 1)) (set! edit_distance_insert_cost (+ (edit_distance edit_distance_source (subs edit_distance_target 0 (min (- (count edit_distance_target) 1) (count edit_distance_target)))) 1)) (set! edit_distance_replace_cost (+ (edit_distance (subs edit_distance_source 0 (min (- (count edit_distance_source) 1) (count edit_distance_source))) (subs edit_distance_target 0 (min (- (count edit_distance_target) 1) (count edit_distance_target)))) edit_distance_delta)) (throw (ex-info "return" {:v (min3 edit_distance_delete_cost edit_distance_insert_cost edit_distance_replace_cost)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_result nil] (do (set! main_result (edit_distance "ATCGCTG" "TAGCTAA")) (println (str main_result)))))

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
