(ns main (:refer-clojure :exclude [perfect_square perfect_square_binary_search]))

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

(declare perfect_square perfect_square_binary_search)

(def ^:dynamic perfect_square_binary_search_left nil)

(def ^:dynamic perfect_square_binary_search_mid nil)

(def ^:dynamic perfect_square_binary_search_right nil)

(def ^:dynamic perfect_square_binary_search_sq nil)

(def ^:dynamic perfect_square_i nil)

(defn perfect_square [perfect_square_num]
  (binding [perfect_square_i nil] (try (do (when (< perfect_square_num 0) (throw (ex-info "return" {:v false}))) (set! perfect_square_i 0) (while (<= (* perfect_square_i perfect_square_i) perfect_square_num) (do (when (= (* perfect_square_i perfect_square_i) perfect_square_num) (throw (ex-info "return" {:v true}))) (set! perfect_square_i (+ perfect_square_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn perfect_square_binary_search [perfect_square_binary_search_n]
  (binding [perfect_square_binary_search_left nil perfect_square_binary_search_mid nil perfect_square_binary_search_right nil perfect_square_binary_search_sq nil] (try (do (when (< perfect_square_binary_search_n 0) (throw (ex-info "return" {:v false}))) (set! perfect_square_binary_search_left 0) (set! perfect_square_binary_search_right perfect_square_binary_search_n) (while (<= perfect_square_binary_search_left perfect_square_binary_search_right) (do (set! perfect_square_binary_search_mid (quot (+ perfect_square_binary_search_left perfect_square_binary_search_right) 2)) (set! perfect_square_binary_search_sq (* perfect_square_binary_search_mid perfect_square_binary_search_mid)) (when (= perfect_square_binary_search_sq perfect_square_binary_search_n) (throw (ex-info "return" {:v true}))) (if (> perfect_square_binary_search_sq perfect_square_binary_search_n) (set! perfect_square_binary_search_right (- perfect_square_binary_search_mid 1)) (set! perfect_square_binary_search_left (+ perfect_square_binary_search_mid 1))))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (perfect_square 9)))
      (println (str (perfect_square 10)))
      (println (str (perfect_square_binary_search 16)))
      (println (str (perfect_square_binary_search 10)))
      (println (str (perfect_square_binary_search (- 1))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
