(ns main (:refer-clojure :exclude [split_with_sep split]))

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

(declare split_with_sep split)

(def ^:dynamic split_with_sep_ch nil)

(def ^:dynamic split_with_sep_i nil)

(def ^:dynamic split_with_sep_last nil)

(def ^:dynamic split_with_sep_parts nil)

(defn split_with_sep [split_with_sep_s split_with_sep_sep]
  (binding [split_with_sep_ch nil split_with_sep_i nil split_with_sep_last nil split_with_sep_parts nil] (try (do (set! split_with_sep_parts []) (set! split_with_sep_last 0) (set! split_with_sep_i 0) (while (< split_with_sep_i (count split_with_sep_s)) (do (set! split_with_sep_ch (subs split_with_sep_s split_with_sep_i (min (+ split_with_sep_i 1) (count split_with_sep_s)))) (when (= split_with_sep_ch split_with_sep_sep) (do (set! split_with_sep_parts (conj split_with_sep_parts (subs split_with_sep_s split_with_sep_last (min split_with_sep_i (count split_with_sep_s))))) (set! split_with_sep_last (+ split_with_sep_i 1)))) (when (= (+ split_with_sep_i 1) (count split_with_sep_s)) (set! split_with_sep_parts (conj split_with_sep_parts (subs split_with_sep_s split_with_sep_last (min (+ split_with_sep_i 1) (count split_with_sep_s)))))) (set! split_with_sep_i (+ split_with_sep_i 1)))) (throw (ex-info "return" {:v split_with_sep_parts}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn split [split_s]
  (try (throw (ex-info "return" {:v (split_with_sep split_s " ")})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (split_with_sep "apple#banana#cherry#orange" "#")))
      (println (str (split "Hello there")))
      (println (str (split_with_sep "11/22/63" "/")))
      (println (str (split_with_sep "12:43:39" ":")))
      (println (str (split_with_sep ";abbb;;c;" ";")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
