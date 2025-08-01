(ns main (:refer-clojure :exclude [chr]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare chr)

(declare b r)

(defn chr [chr_n]
  (try (do (when (= chr_n 97) (throw (ex-info "return" {:v "a"}))) (when (= chr_n 960) (throw (ex-info "return" {:v "π"}))) (if (= chr_n 65) "A" "?")) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def b 97)

(def r 960)

(defn -main []
  (println (str (str (chr 97) " ") (chr 960)))
  (println (str (str (chr b) " ") (chr r))))

(-main)
