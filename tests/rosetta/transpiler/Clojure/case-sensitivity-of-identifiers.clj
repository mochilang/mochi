(ns main (:refer-clojure :exclude [main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare main)

(declare DOG d dog main_Dog main_pkg_DOG main_pkg_dog)

(defn packageSees [packageSees_d1 packageSees_d2 packageSees_d3]
  (try (do (println (str (str (str (str (str "Package sees: " packageSees_d1) " ") packageSees_d2) " ") packageSees_d3)) (throw (ex-info "return" {:v {"pkg_dog" true "Dog" true "pkg_DOG" true}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (try (do (def main_pkg_dog "Salt") (def main_Dog "Pepper") (def main_pkg_DOG "Mustard") (def d (packageSees pkg_dog Dog pkg_DOG)) (println (str (str "There are " (str (count d))) " dogs.\n")) (def dog "Benjamin") (def d (packageSees pkg_dog Dog pkg_DOG)) (println (str (str (str (str (str "Main sees:   " dog) " ") Dog) " ") pkg_DOG)) (def d (assoc d "dog" true)) (def d (assoc d "Dog" true)) (def d (assoc d "pkg_DOG" true)) (println (str (str "There are " (str (count d))) " dogs.\n")) (def Dog "Samba") (def d (packageSees pkg_dog Dog pkg_DOG)) (println (str (str (str (str (str "Main sees:   " dog) " ") Dog) " ") pkg_DOG)) (def d (assoc d "dog" true)) (def d (assoc d "Dog" true)) (def d (assoc d "pkg_DOG" true)) (println (str (str "There are " (str (count d))) " dogs.\n")) (def DOG "Bernie") (def d (packageSees pkg_dog Dog pkg_DOG)) (println (str (str (str (str (str "Main sees:   " dog) " ") Dog) " ") DOG)) (def d (assoc d "dog" true)) (def d (assoc d "Dog" true)) (def d (assoc d "pkg_DOG" true)) (def d (assoc d "DOG" true)) (println (str (str "There are " (str (count d))) " dogs."))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (main))

(-main)
