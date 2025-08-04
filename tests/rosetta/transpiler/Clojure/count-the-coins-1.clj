(ns main (:refer-clojure :exclude [countChange]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare countChange)

(def ^:dynamic countChange_coin nil)

(def ^:dynamic countChange_coins nil)

(def ^:dynamic countChange_i nil)

(def ^:dynamic countChange_idx nil)

(def ^:dynamic countChange_j nil)

(def ^:dynamic countChange_ways nil)

(defn countChange [countChange_amount]
  (binding [countChange_coin nil countChange_coins nil countChange_i nil countChange_idx nil countChange_j nil countChange_ways nil] (try (do (set! countChange_ways []) (set! countChange_i 0) (while (<= countChange_i countChange_amount) (do (set! countChange_ways (conj countChange_ways 0)) (set! countChange_i (+ countChange_i 1)))) (set! countChange_ways (assoc countChange_ways 0 1)) (set! countChange_coins [1 5 10 25]) (set! countChange_idx 0) (while (< countChange_idx (count countChange_coins)) (do (set! countChange_coin (nth countChange_coins countChange_idx)) (set! countChange_j countChange_coin) (while (<= countChange_j countChange_amount) (do (set! countChange_ways (assoc countChange_ways countChange_j (+ (nth countChange_ways countChange_j) (nth countChange_ways (- countChange_j countChange_coin))))) (set! countChange_j (+ countChange_j 1)))) (set! countChange_idx (+ countChange_idx 1)))) (throw (ex-info "return" {:v (nth countChange_ways countChange_amount)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_amount 10)

(defn -main []
  (println (str (str (str "amount, ways to make change: " (str main_amount)) " ") (str (countChange main_amount)))))

(-main)
