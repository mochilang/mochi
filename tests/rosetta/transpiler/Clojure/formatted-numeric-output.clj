(ns main (:refer-clojure :exclude [pow10 formatFloat padLeftZeros]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare pow10 formatFloat padLeftZeros)

(declare digits fracPart i intPart n out r scale scaled)

(defn pow10 [n]
  (try (do (def r 1.0) (def i 0) (while (< i n) (do (def r (* r 10.0)) (def i (+' i 1)))) (throw (ex-info "return" {:v r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn formatFloat [f prec]
  (try (do (def scale (pow10 prec)) (def scaled (+' (* f scale) 0.5)) (def n (int scaled)) (def digits (str n)) (while (<= (count digits) prec) (def digits (str "0" digits))) (def intPart (subs digits 0 (- (count digits) prec))) (def fracPart (subs digits (- (count digits) prec) (count digits))) (throw (ex-info "return" {:v (str (str intPart ".") fracPart)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn padLeftZeros [s width]
  (try (do (def out s) (while (< (count out) width) (def out (str "0" out))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (println (padLeftZeros (formatFloat 7.125 3) 9)))

(-main)
