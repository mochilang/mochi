(ns main (:refer-clojure :exclude [get_mid point_to_string triangle]))

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

(declare get_mid point_to_string triangle)

(defn get_mid [get_mid_p1 get_mid_p2]
  (try (throw (ex-info "return" {:v {:x (quot (+ (:x get_mid_p1) (:x get_mid_p2)) 2) :y (quot (+ (:y get_mid_p1) (:y get_mid_p2)) 2)}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn point_to_string [point_to_string_p]
  (try (throw (ex-info "return" {:v (str (str (str (str "(" (str (:x point_to_string_p))) ",") (str (:y point_to_string_p))) ")")})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn triangle [triangle_v1 triangle_v2 triangle_v3 triangle_depth]
  (try (do (println (str (str (str (str (point_to_string triangle_v1) " ") (point_to_string triangle_v2)) " ") (point_to_string triangle_v3))) (when (= triangle_depth 0) (throw (ex-info "return" {:v nil}))) (triangle triangle_v1 (get_mid triangle_v1 triangle_v2) (get_mid triangle_v1 triangle_v3) (- triangle_depth 1)) (triangle triangle_v2 (get_mid triangle_v1 triangle_v2) (get_mid triangle_v2 triangle_v3) (- triangle_depth 1)) (triangle triangle_v3 (get_mid triangle_v3 triangle_v2) (get_mid triangle_v1 triangle_v3) (- triangle_depth 1))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (triangle {:x (- 175) :y (- 125)} {:x 0 :y 175} {:x 175 :y (- 125)} 2)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
