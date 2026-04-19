(ns main
  (:require [clojure.string :as str]))

(def letters (map char (range (int \a) (inc (int \z)))))

(defn neighbors [word]
  (let [chars (vec word)]
    (for [i (range (count chars))
          c letters
          :when (not= c (chars i))]
      (apply str (assoc chars i c)))))

(defn add-parent [parents child parent]
  (update parents child (fnil conj []) parent))

(defn ladders [begin-word end-word words]
  (let [word-set (set words)]
    (if (not (contains? word-set end-word))
      []
      (loop [level #{begin-word}
             visited #{begin-word}
             parents {}
             found false]
        (if (or (empty? level) found)
          (if found
            (letfn [(build [word]
                      (if (= word begin-word)
                        [[begin-word]]
                        (mapcat
                         (fn [p]
                           (map #(conj % word) (build p)))
                         (sort (get parents word [])))))]
              (sort (build end-word)))
            [])
          (let [{next-level :next-level next-parents :parents next-found :found}
                (reduce
                 (fn [{:keys [next-level parents found]} word]
                   (reduce
                    (fn [{:keys [next-level parents found]} nw]
                      (if (and (contains? word-set nw)
                               (not (contains? visited nw)))
                        {:next-level (conj next-level nw)
                         :parents (add-parent parents nw word)
                         :found (or found (= nw end-word))}
                        {:next-level next-level :parents parents :found found}))
                    {:next-level next-level :parents parents :found found}
                    (neighbors word)))
                 {:next-level #{} :parents parents :found false}
                 (sort level))]
            (recur next-level (into visited next-level) next-parents next-found)))))))

(defn fmt [paths]
  (str/join "\n" (cons (str (count paths)) (map #(str/join "->" %) paths))))

(defn -main []
  (let [lines (str/split-lines (slurp *in*))]
    (when (seq lines)
      (let [tc (Integer/parseInt (first lines))]
        (loop [t 0 idx 1 out []]
          (if (= t tc)
            (print (str/join "\n\n" out))
            (let [begin-word (nth lines idx)
                  end-word (nth lines (inc idx))
                  n (Integer/parseInt (nth lines (+ idx 2)))
                  words (subvec (vec lines) (+ idx 3) (+ idx 3 n))]
              (recur (inc t)
                     (+ idx 3 n)
                     (conj out (fmt (ladders begin-word end-word words)))))))))))

(-main)
