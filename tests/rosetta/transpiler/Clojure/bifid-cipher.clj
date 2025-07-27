(ns main (:refer-clojure :exclude [square_to_maps remove_space encrypt decrypt main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare square_to_maps remove_space encrypt decrypt main)

(defn square_to_maps [square]
  (try (do (def emap {}) (def dmap {}) (def x 0) (while (< x (count square)) (do (def row (nth square x)) (def y 0) (while (< y (count row)) (do (def ch (nth row y)) (def emap (assoc emap ch [x y])) (def dmap (assoc dmap (str (str (str x) ",") (str y)) ch)) (def y (+ y 1)))) (def x (+ x 1)))) (throw (ex-info "return" {:v {"e" emap "d" dmap}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn remove_space [text emap]
  (try (do (def s (clojure.string/upper-case text)) (def out "") (def i 0) (while (< i (count s)) (do (def ch (subvec s i (+ i 1))) (when (and (not= ch " ") (in ch emap)) (def out (str out ch))) (def i (+ i 1)))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn encrypt [text emap dmap]
  (try (do (def text (remove_space text emap)) (def row0 []) (def row1 []) (def i 0) (while (< i (count text)) (do (def ch (subs text i (+ i 1))) (def xy (get emap ch)) (def row0 (conj row0 (nth xy 0))) (def row1 (conj row1 (nth xy 1))) (def i (+ i 1)))) (doseq [v row1] (def row0 (conj row0 v))) (def res "") (def j 0) (while (< j (count row0)) (do (def key (str (str (str (nth row0 j)) ",") (str (nth row0 (+ j 1))))) (def res (str res (get dmap key))) (def j (+ j 2)))) (throw (ex-info "return" {:v res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn decrypt [text emap dmap]
  (try (do (def text (remove_space text emap)) (def coords []) (def i 0) (while (< i (count text)) (do (def ch (subs text i (+ i 1))) (def xy (get emap ch)) (def coords (conj coords (nth xy 0))) (def coords (conj coords (nth xy 1))) (def i (+ i 1)))) (def half (/ (count coords) 2)) (def k1 []) (def k2 []) (def idx 0) (while (< idx half) (do (def k1 (conj k1 (nth coords idx))) (def idx (+ idx 1)))) (while (< idx (count coords)) (do (def k2 (conj k2 (nth coords idx))) (def idx (+ idx 1)))) (def res "") (def j 0) (while (< j half) (do (def key (str (str (str (nth k1 j)) ",") (str (nth k2 j)))) (def res (str res (get dmap key))) (def j (+ j 1)))) (throw (ex-info "return" {:v res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def squareRosetta [["A" "B" "C" "D" "E"] ["F" "G" "H" "I" "K"] ["L" "M" "N" "O" "P"] ["Q" "R" "S" "T" "U"] ["V" "W" "X" "Y" "Z"] ["J" "1" "2" "3" "4"]]) (def squareWikipedia [["B" "G" "W" "K" "Z"] ["Q" "P" "N" "D" "S"] ["I" "O" "A" "X" "E"] ["F" "C" "L" "U" "M"] ["T" "H" "Y" "V" "R"] ["J" "1" "2" "3" "4"]]) (def textRosetta "0ATTACKATDAWN") (def textWikipedia "FLEEATONCE") (def textTest "The invasion will start on the first of January") (def maps (square_to_maps squareRosetta)) (def emap (get maps "e")) (def dmap (get maps "d")) (println "from Rosettacode") (println (str "original:\t " textRosetta)) (def s (encrypt textRosetta emap dmap)) (println (str "codiert:\t " s)) (def s (decrypt s emap dmap)) (println (str "and back:\t " s)) (def maps (square_to_maps squareWikipedia)) (def emap (get maps "e")) (def dmap (get maps "d")) (println "from Wikipedia") (println (str "original:\t " textWikipedia)) (def s (encrypt textWikipedia emap dmap)) (println (str "codiert:\t " s)) (def s (decrypt s emap dmap)) (println (str "and back:\t " s)) (def maps (square_to_maps squareWikipedia)) (def emap (get maps "e")) (def dmap (get maps "d")) (println "from Rosettacode long part") (println (str "original:\t " textTest)) (def s (encrypt textTest emap dmap)) (println (str "codiert:\t " s)) (def s (decrypt s emap dmap)) (println (str "and back:\t " s))))

(defn -main []
  (main))

(-main)
