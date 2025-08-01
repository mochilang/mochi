(ns main (:refer-clojure :exclude [listStr llStr cartN main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare listStr llStr cartN main)

(declare cartN_a cartN_c cartN_idx cartN_j cartN_k cartN_n cartN_res cartN_row count_v listStr_i listStr_s llStr_i llStr_s)

(defn listStr [listStr_xs]
  (try (do (def listStr_s "[") (def listStr_i 0) (while (< listStr_i (count listStr_xs)) (do (def listStr_s (str listStr_s (str (nth listStr_xs listStr_i)))) (when (< listStr_i (- (count listStr_xs) 1)) (def listStr_s (str listStr_s " "))) (def listStr_i (+ listStr_i 1)))) (def listStr_s (str listStr_s "]")) (throw (ex-info "return" {:v listStr_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn llStr [llStr_lst]
  (try (do (def llStr_s "[") (def llStr_i 0) (while (< llStr_i (count llStr_lst)) (do (def llStr_s (str llStr_s (listStr (nth llStr_lst llStr_i)))) (when (< llStr_i (- (count llStr_lst) 1)) (def llStr_s (str llStr_s " "))) (def llStr_i (+ llStr_i 1)))) (def llStr_s (str llStr_s "]")) (throw (ex-info "return" {:v llStr_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn cartN [cartN_lists]
  (try (do (when (= cartN_lists nil) (throw (ex-info "return" {:v []}))) (def cartN_a cartN_lists) (when (= (count cartN_a) 0) (throw (ex-info "return" {:v [[]]}))) (def cartN_c 1) (doseq [xs cartN_a] (def cartN_c (* cartN_c (count cartN_xs)))) (when (= cartN_c 0) (throw (ex-info "return" {:v []}))) (def cartN_res []) (def cartN_idx []) (doseq [_ cartN_a] (def cartN_idx (conj cartN_idx 0))) (def cartN_n (count cartN_a)) (def count_v 0) (while (< count_v cartN_c) (do (def cartN_row []) (def cartN_j 0) (while (< cartN_j cartN_n) (do (def cartN_row (conj cartN_row (nth (nth cartN_a cartN_j) (nth cartN_idx cartN_j)))) (def cartN_j (+ cartN_j 1)))) (def cartN_res (conj cartN_res cartN_row)) (def cartN_k (- cartN_n 1)) (loop [while_flag_1 true] (when (and while_flag_1 (>= cartN_k 0)) (do (def cartN_idx (assoc cartN_idx cartN_k (+ (nth cartN_idx cartN_k) 1))) (cond (< (nth cartN_idx cartN_k) (count (nth cartN_a cartN_k))) (recur false) :else (do (def cartN_idx (assoc cartN_idx cartN_k 0)) (def cartN_k (- cartN_k 1)) (recur while_flag_1)))))) (def count_v (+ count_v 1)))) (throw (ex-info "return" {:v cartN_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (println (llStr (cartN [[1 2] [3 4]]))) (println (llStr (cartN [[3 4] [1 2]]))) (println (llStr (cartN [[1 2] []]))) (println (llStr (cartN [[] [1 2]]))) (println "") (println "[") (doseq [p (cartN [[1776 1789] [7 12] [4 14 23] [0 1]])] (println (str " " (listStr main_p)))) (println "]") (println (llStr (cartN [[1 2 3] [30] [500 100]]))) (println (llStr (cartN [[1 2 3] [] [500 100]]))) (println "") (println (llStr (cartN nil))) (println (llStr (cartN [])))))

(defn -main []
  (main))

(-main)
