(ns main (:refer-clojure :exclude [perfect_cube perfect_cube_binary_search]))

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

(declare perfect_cube perfect_cube_binary_search)

(def ^:dynamic perfect_cube_binary_search_cube nil)

(def ^:dynamic perfect_cube_binary_search_left nil)

(def ^:dynamic perfect_cube_binary_search_m nil)

(def ^:dynamic perfect_cube_binary_search_mid nil)

(def ^:dynamic perfect_cube_binary_search_right nil)

(def ^:dynamic perfect_cube_i nil)

(def ^:dynamic perfect_cube_m nil)

(defn perfect_cube [perfect_cube_n]
  (binding [perfect_cube_i nil perfect_cube_m nil] (try (do (set! perfect_cube_m perfect_cube_n) (when (< perfect_cube_m 0) (set! perfect_cube_m (- perfect_cube_m))) (set! perfect_cube_i 0) (while (< (* (* perfect_cube_i perfect_cube_i) perfect_cube_i) perfect_cube_m) (set! perfect_cube_i (+ perfect_cube_i 1))) (throw (ex-info "return" {:v (= (* (* perfect_cube_i perfect_cube_i) perfect_cube_i) perfect_cube_m)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn perfect_cube_binary_search [perfect_cube_binary_search_n]
  (binding [perfect_cube_binary_search_cube nil perfect_cube_binary_search_left nil perfect_cube_binary_search_m nil perfect_cube_binary_search_mid nil perfect_cube_binary_search_right nil] (try (do (set! perfect_cube_binary_search_m perfect_cube_binary_search_n) (when (< perfect_cube_binary_search_m 0) (set! perfect_cube_binary_search_m (- perfect_cube_binary_search_m))) (set! perfect_cube_binary_search_left 0) (set! perfect_cube_binary_search_right perfect_cube_binary_search_m) (while (<= perfect_cube_binary_search_left perfect_cube_binary_search_right) (do (set! perfect_cube_binary_search_mid (+ perfect_cube_binary_search_left (quot (- perfect_cube_binary_search_right perfect_cube_binary_search_left) 2))) (set! perfect_cube_binary_search_cube (* (* perfect_cube_binary_search_mid perfect_cube_binary_search_mid) perfect_cube_binary_search_mid)) (when (= perfect_cube_binary_search_cube perfect_cube_binary_search_m) (throw (ex-info "return" {:v true}))) (if (< perfect_cube_binary_search_cube perfect_cube_binary_search_m) (set! perfect_cube_binary_search_left (+ perfect_cube_binary_search_mid 1)) (set! perfect_cube_binary_search_right (- perfect_cube_binary_search_mid 1))))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (perfect_cube 27)))
      (println (str (perfect_cube 4)))
      (println (str (perfect_cube_binary_search 27)))
      (println (str (perfect_cube_binary_search 64)))
      (println (str (perfect_cube_binary_search 4)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
