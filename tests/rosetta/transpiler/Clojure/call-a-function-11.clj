(ns main (:refer-clojure :exclude [zeroval zeroptr main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare zeroval zeroptr main)

(declare main_box main_i main_tmp zeroval_x)

(defn zeroval [zeroval_ival]
  (try (do (def zeroval_x zeroval_ival) (def zeroval_x 0) (throw (ex-info "return" {:v zeroval_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn zeroptr [zeroptr_ref_p]
  (do (def zeroptr_ref zeroptr_ref_p) (def zeroptr_ref (assoc zeroptr_ref 0 0))))

(defn main []
  (do (def main_i 1) (println (str "initial: " (str main_i))) (def main_tmp (zeroval main_i)) (println (str "zeroval: " (str main_i))) (def main_box [main_i]) (zeroptr main_box) (def main_i (nth main_box 0)) (println (str "zeroptr: " (str main_i))) (println "pointer: 0")))

(defn -main []
  (main))

(-main)
