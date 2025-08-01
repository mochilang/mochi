(ns main (:refer-clojure :exclude [mapString main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare mapString main)

(declare main_fn mapString_i mapString_out)

(defn mapString [mapString_s mapString_f]
  (try (do (def mapString_out "") (def mapString_i 0) (while (< mapString_i (count mapString_s)) (do (def mapString_out (+ mapString_out (f (subs mapString_s mapString_i (+ mapString_i 1))))) (def mapString_i (+ mapString_i 1)))) (throw (ex-info "return" {:v mapString_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def main_fn (fn [r] (if (= main_r " ") "" main_r))) (mapString "Spaces removed" main_fn) (mapString "Test" (fn [r] (clojure.string/lower-case main_r))) (mapString "shift" (fn [r] main_r))))

(defn -main []
  (main))

(-main)
