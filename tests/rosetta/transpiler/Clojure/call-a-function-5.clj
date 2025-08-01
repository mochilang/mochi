(ns main (:refer-clojure :exclude [doIt main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare doIt main)

(declare doIt_b main_p)

(defn doIt [doIt_p]
  (try (do (def doIt_b 0) (when (in "b" doIt_p) (def doIt_b (get doIt_p "b"))) (throw (ex-info "return" {:v (+ (+ (get doIt_p "a") doIt_b) (get doIt_p "c"))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def main_p {}) (def main_p (assoc main_p "a" 1)) (def main_p (assoc main_p "c" 9)) (println (str (doIt main_p)))))

(defn -main []
  (main))

(-main)
