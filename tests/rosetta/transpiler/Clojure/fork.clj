(ns main (:refer-clojure :exclude [run]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare run)

(declare childPID nextPID pid)

(def nextPID 1)

(defn run [hasArg]
  (try (do (def pid nextPID) (def nextPID (+' nextPID 1)) (println (str "PID: " (str pid))) (when (not hasArg) (do (println "Done.") (throw (ex-info "return" {:v nil})))) (def childPID nextPID) (println (str "Child's PID: " (str childPID))) (run false)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (run true))

(-main)
