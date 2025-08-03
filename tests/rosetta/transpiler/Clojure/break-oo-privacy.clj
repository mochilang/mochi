(ns main (:refer-clojure :exclude [examineAndModify anotherExample]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare examineAndModify anotherExample)

(declare main_obj)

(defn examineAndModify [examineAndModify_f_p]
  (try (do (def examineAndModify_f examineAndModify_f_p) (println (str (str (str (str (str (str (str (str " v: {" (str (:Exported examineAndModify_f))) " ") (str (:unexported examineAndModify_f))) "} = {") (str (:Exported examineAndModify_f))) " ") (str (:unexported examineAndModify_f))) "}")) (println "    Idx Name       Type CanSet") (println "     0: Exported   int  true") (println "     1: unexported int  false") (def examineAndModify_f (assoc examineAndModify_f :Exported 16)) (def examineAndModify_f (assoc examineAndModify_f :unexported 44)) (println "  modified unexported field via unsafe") (throw (ex-info "return" {:v examineAndModify_f}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn anotherExample []
  (println "bufio.ReadByte returned error: unsafely injected error value into bufio inner workings"))

(def main_obj {:Exported 12 :unexported 42})

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (str (str (str "obj: {" (str (:Exported main_obj))) " ") (str (:unexported main_obj))) "}"))
      (def main_obj (examineAndModify main_obj))
      (println (str (str (str (str "obj: {" (str (:Exported main_obj))) " ") (str (:unexported main_obj))) "}"))
      (anotherExample)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
