(ns main (:refer-clojure :exclude [fields canSpell newSpeller main]))

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare fields canSpell newSpeller main)

(defn fields [s]
  (try (do (def res []) (def cur "") (def i 0) (while (< i (count s)) (do (def c (subs s i (+ i 1))) (if (= c " ") (when (> (count cur) 0) (do (def res (conj res cur)) (def cur ""))) (def cur (str cur c))) (def i (+ i 1)))) (when (> (count cur) 0) (def res (conj res cur))) (throw (ex-info "return" {:v res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn canSpell [word blks]
  (try (do (when (= (count word) 0) (throw (ex-info "return" {:v true}))) (def c (lower (subs word 0 1))) (def i 0) (while (< i (count blks)) (do (def b (nth blks i)) (when (or (= c (lower (subvec b 0 1))) (= c (lower (subvec b 1 2)))) (do (def rest []) (def j 0) (while (< j (count blks)) (do (when (not= j i) (def rest (conj rest (nth blks j)))) (def j (+ j 1)))) (when (canSpell (subs word 1 (count word)) rest) (throw (ex-info "return" {:v true}))))) (def i (+ i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn newSpeller [blocks]
  (try (do (def bl (fields blocks)) (throw (ex-info "return" {:v (fn [w] (canSpell w bl))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def sp (newSpeller "BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM")) (doseq [word ["A" "BARK" "BOOK" "TREAT" "COMMON" "SQUAD" "CONFUSE"]] (println (str (str word " ") (str (sp word)))))))

(defn -main []
  (main))

(-main)
