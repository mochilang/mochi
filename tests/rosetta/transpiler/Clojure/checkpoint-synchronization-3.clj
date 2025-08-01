(ns main (:refer-clojure :exclude [lower]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare lower)

(declare a lower_i lower_low lower_up nAssemblies partList)

(defn lower [lower_ch]
  (try (do (def lower_up "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (def lower_low "abcdefghijklmnopqrstuvwxyz") (def lower_i 0) (while (< lower_i (count lower_up)) (do (when (= lower_ch (subs lower_up lower_i (+ lower_i 1))) (throw (ex-info "return" {:v (subs lower_low lower_i (+ lower_i 1))}))) (def lower_i (+ lower_i 1)))) (throw (ex-info "return" {:v lower_ch}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def partList ["A" "B" "C" "D"])

(def nAssemblies 3)

(defn -main []
  (doseq [p partList] (println (str p " worker running")))
  (doseq [cycle (range 1 (+ nAssemblies 1))] (do (println (str "begin assembly cycle " (str cycle))) (def a "") (doseq [p partList] (do (println (str p " worker begins part")) (println (str (str p " worker completed ") (lower p))) (def a (str a (lower p))))) (println (str (str (str a " assembled.  cycle ") (str cycle)) " complete"))))
  (doseq [p partList] (println (str p " worker stopped"))))

(-main)
