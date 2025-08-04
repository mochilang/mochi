(ns main (:refer-clojure :exclude [cart2 llStr main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare cart2 llStr main)

(declare cart2_p llStr_i llStr_j llStr_row llStr_s)

(defn cart2 [cart2_a cart2_b]
  (try (do (def cart2_p []) (doseq [x cart2_a] (doseq [y cart2_b] (def cart2_p (conj cart2_p [x y])))) (throw (ex-info "return" {:v cart2_p}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn llStr [llStr_lst]
  (try (do (def llStr_s "[") (def llStr_i 0) (while (< llStr_i (count llStr_lst)) (do (def llStr_row (nth llStr_lst llStr_i)) (def llStr_s (str llStr_s "[")) (def llStr_j 0) (while (< llStr_j (count llStr_row)) (do (def llStr_s (str llStr_s (str (nth llStr_row llStr_j)))) (when (< llStr_j (- (count llStr_row) 1)) (def llStr_s (str llStr_s " "))) (def llStr_j (+ llStr_j 1)))) (def llStr_s (str llStr_s "]")) (when (< llStr_i (- (count llStr_lst) 1)) (def llStr_s (str llStr_s " "))) (def llStr_i (+ llStr_i 1)))) (def llStr_s (str llStr_s "]")) (throw (ex-info "return" {:v llStr_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (println (llStr (cart2 [1 2] [3 4]))) (println (llStr (cart2 [3 4] [1 2]))) (println (llStr (cart2 [1 2] []))) (println (llStr (cart2 [] [1 2])))))

(defn -main []
  (main))

(-main)
