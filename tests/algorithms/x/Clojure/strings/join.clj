(ns main (:refer-clojure :exclude [join main]))

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

(declare join main)

(def ^:dynamic join_i nil)

(def ^:dynamic join_joined nil)

(def ^:dynamic join_last_index nil)

(defn join [join_separator join_separated]
  (binding [join_i nil join_joined nil join_last_index nil] (try (do (set! join_joined "") (set! join_last_index (- (count join_separated) 1)) (set! join_i 0) (while (< join_i (count join_separated)) (do (set! join_joined (str join_joined (nth join_separated join_i))) (when (< join_i join_last_index) (set! join_joined (str join_joined join_separator))) (set! join_i (+ join_i 1)))) (throw (ex-info "return" {:v join_joined}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (join "" ["a" "b" "c" "d"])) (println (join "#" ["a" "b" "c" "d"])) (println (join "#" ["a"])) (println (join " " ["You" "are" "amazing!"])) (println (join "," ["" "" ""])) (println (join "-" ["apple" "banana" "cherry"]))))

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
