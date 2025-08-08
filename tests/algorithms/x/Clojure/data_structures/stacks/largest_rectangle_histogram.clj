(ns main (:refer-clojure :exclude [largest_rectangle_area]))

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

(declare largest_rectangle_area)

(def ^:dynamic largest_rectangle_area_area nil)

(def ^:dynamic largest_rectangle_area_height nil)

(def ^:dynamic largest_rectangle_area_hs nil)

(def ^:dynamic largest_rectangle_area_i nil)

(def ^:dynamic largest_rectangle_area_max_area nil)

(def ^:dynamic largest_rectangle_area_stack nil)

(def ^:dynamic largest_rectangle_area_top nil)

(def ^:dynamic largest_rectangle_area_width nil)

(defn largest_rectangle_area [largest_rectangle_area_heights]
  (binding [largest_rectangle_area_area nil largest_rectangle_area_height nil largest_rectangle_area_hs nil largest_rectangle_area_i nil largest_rectangle_area_max_area nil largest_rectangle_area_stack nil largest_rectangle_area_top nil largest_rectangle_area_width nil] (try (do (set! largest_rectangle_area_stack []) (set! largest_rectangle_area_max_area 0) (set! largest_rectangle_area_hs largest_rectangle_area_heights) (set! largest_rectangle_area_hs (conj largest_rectangle_area_hs 0)) (set! largest_rectangle_area_i 0) (while (< largest_rectangle_area_i (count largest_rectangle_area_hs)) (do (while (and (> (count largest_rectangle_area_stack) 0) (< (nth largest_rectangle_area_hs largest_rectangle_area_i) (nth largest_rectangle_area_hs (nth largest_rectangle_area_stack (- (count largest_rectangle_area_stack) 1))))) (do (set! largest_rectangle_area_top (nth largest_rectangle_area_stack (- (count largest_rectangle_area_stack) 1))) (set! largest_rectangle_area_stack (subvec largest_rectangle_area_stack 0 (- (count largest_rectangle_area_stack) 1))) (set! largest_rectangle_area_height (nth largest_rectangle_area_hs largest_rectangle_area_top)) (set! largest_rectangle_area_width largest_rectangle_area_i) (when (> (count largest_rectangle_area_stack) 0) (set! largest_rectangle_area_width (- (- largest_rectangle_area_i (nth largest_rectangle_area_stack (- (count largest_rectangle_area_stack) 1))) 1))) (set! largest_rectangle_area_area (* largest_rectangle_area_height largest_rectangle_area_width)) (when (> largest_rectangle_area_area largest_rectangle_area_max_area) (set! largest_rectangle_area_max_area largest_rectangle_area_area)))) (set! largest_rectangle_area_stack (conj largest_rectangle_area_stack largest_rectangle_area_i)) (set! largest_rectangle_area_i (+ largest_rectangle_area_i 1)))) (throw (ex-info "return" {:v largest_rectangle_area_max_area}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (largest_rectangle_area [2 1 5 6 2 3])))
      (println (str (largest_rectangle_area [2 4])))
      (println (str (largest_rectangle_area [6 2 5 4 5 1 6])))
      (println (str (largest_rectangle_area [1])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
