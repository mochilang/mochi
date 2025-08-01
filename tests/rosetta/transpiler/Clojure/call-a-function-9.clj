(ns main (:refer-clojure :exclude [f g h main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare f g h main)

(declare main_a main_ab main_b main_cb main_d main_e main_i main_list)

(defn f []
  (try (throw (ex-info "return" {:v [0 0.0]})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn g [g_a g_b]
  (try (throw (ex-info "return" {:v 0})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn h [h_s h_nums]
  (do))

(defn main []
  (do (def main_ab (f)) (def main_a (nth main_ab 0)) (def main_b (nth main_ab 1)) (def main_cb (nth (f) 1)) (def main_d (g main_a main_cb)) (def main_e (g main_d main_b)) (def main_i (g main_d 2.0)) (def main_list []) (def main_list (conj main_list main_a)) (def main_list (conj main_list main_d)) (def main_list (conj main_list main_e)) (def main_list (conj main_list main_i)) (def main_i (count main_list))))

(defn -main []
  (main))

(-main)
