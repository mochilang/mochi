(ns main (:refer-clojure :exclude [amb main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare amb main)

(defn amb [wordsets res idx]
  (try (do (when (= idx (count wordsets)) (throw (ex-info "return" {:v true}))) (def prev "") (when (> idx 0) (def prev (nth res (- idx 1)))) (def i 0) (while (< i (count (nth wordsets idx))) (do (def w (nth (nth wordsets idx) i)) (when (or (= idx 0) (= (subs prev (- (count prev) 1) (count prev)) (subs w 0 1))) (do (def res (assoc res idx w)) (when (amb wordsets res (+ idx 1)) (throw (ex-info "return" {:v true}))))) (def i (+ i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def wordset [["the" "that" "a"] ["frog" "elephant" "thing"] ["walked" "treaded" "grows"] ["slowly" "quickly"]]) (def res []) (def i 0) (while (< i (count wordset)) (do (def res (conj res "")) (def i (+ i 1)))) (if (amb wordset res 0) (do (def out (str "[" (nth res 0))) (def j 1) (while (< j (count res)) (do (def out (str (str out " ") (nth res j))) (def j (+ j 1)))) (def out (str out "]")) (println out)) (println "No amb found"))))

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
