(ns main (:refer-clojure :exclude [beastKind beastName beastCry bprint main]))

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(defn beastKind [b]
  (try (throw (ex-info "return" {:v (cond true (let [k (:kind b) _ (:name b)] k) true (let [k (:kind b) _ (:name b)] k))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn beastName [b]
  (try (throw (ex-info "return" {:v (cond true (let [_ (:kind b) n (:name b)] n) true (let [_ (:kind b) n (:name b)] n))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn beastCry [b]
  (try (throw (ex-info "return" {:v (cond true (let [_ (:kind b) _ (:name b)] "Woof") true (let [_ (:kind b) _ (:name b)] "Meow"))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bprint [b]
  (println (str (str (str (str (str (beastName b) ", who's a ") (beastKind b)) ", cries: \"") (beastCry b)) "\".")))

(defn main []
  (do (def d {:kind "labrador" :name "Max"}) (def c {:kind "siamese" :name "Sammy"}) (bprint d) (bprint c)))

(defn -main []
  (main))

(-main)
