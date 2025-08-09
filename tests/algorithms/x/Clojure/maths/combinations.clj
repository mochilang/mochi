(ns main (:refer-clojure :exclude [combinations]))

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

(declare combinations)

(def ^:dynamic combinations_i nil)

(def ^:dynamic combinations_res nil)

(defn combinations [combinations_n combinations_k]
  (binding [combinations_i nil combinations_res nil] (try (do (when (or (< combinations_k 0) (< combinations_n combinations_k)) (throw (Exception. "Please enter positive integers for n and k where n >= k"))) (set! combinations_res 1) (set! combinations_i 0) (while (< combinations_i combinations_k) (do (set! combinations_res (* combinations_res (- combinations_n combinations_i))) (set! combinations_res (quot combinations_res (+ combinations_i 1))) (set! combinations_i (+ combinations_i 1)))) (throw (ex-info "return" {:v combinations_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "The number of five-card hands possible from a standard fifty-two card deck is: " (str (combinations 52 5))))
      (println "")
      (println (str (str "If a class of 40 students must be arranged into groups of 4 for group projects, there are " (str (combinations 40 4))) " ways to arrange them."))
      (println "")
      (println (str (str "If 10 teams are competing in a Formula One race, there are " (str (combinations 10 3))) " ways that first, second and third place can be awarded."))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
