(ns main (:refer-clojure :exclude [main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare main)

(def ^:dynamic main_DOG nil)

(def ^:dynamic main_Dog nil)

(def ^:dynamic main_d nil)

(def ^:dynamic main_dog nil)

(def ^:dynamic main_pkg_DOG nil)

(def ^:dynamic main_pkg_dog nil)

(defn packageSees [packageSees_d1 packageSees_d2 packageSees_d3]
  (try (do (println (str (str (str (str (str "Package sees: " packageSees_d1) " ") packageSees_d2) " ") packageSees_d3)) (throw (ex-info "return" {:v {"pkg_dog" true "Dog" true "pkg_DOG" true}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (binding [main_DOG nil main_Dog nil main_d nil main_dog nil main_pkg_DOG nil main_pkg_dog nil] (try (do (set! main_pkg_dog "Salt") (set! main_Dog "Pepper") (set! main_pkg_DOG "Mustard") (set! main_d (packageSees main_pkg_dog main_Dog main_pkg_DOG)) (println (str (str "There are " (str (count main_d))) " dogs.\n")) (set! main_dog "Benjamin") (set! main_d (packageSees main_pkg_dog main_Dog main_pkg_DOG)) (println (str (str (str (str (str "Main sees:   " main_dog) " ") main_Dog) " ") main_pkg_DOG)) (set! main_d (assoc main_d "dog" true)) (set! main_d (assoc main_d "Dog" true)) (set! main_d (assoc main_d "pkg_DOG" true)) (println (str (str "There are " (str (count main_d))) " dogs.\n")) (set! main_Dog "Samba") (set! main_d (packageSees main_pkg_dog main_Dog main_pkg_DOG)) (println (str (str (str (str (str "Main sees:   " main_dog) " ") main_Dog) " ") main_pkg_DOG)) (set! main_d (assoc main_d "dog" true)) (set! main_d (assoc main_d "Dog" true)) (set! main_d (assoc main_d "pkg_DOG" true)) (println (str (str "There are " (str (count main_d))) " dogs.\n")) (set! main_DOG "Bernie") (set! main_d (packageSees main_pkg_dog main_Dog main_pkg_DOG)) (println (str (str (str (str (str "Main sees:   " main_dog) " ") main_Dog) " ") main_DOG)) (set! main_d (assoc main_d "dog" true)) (set! main_d (assoc main_d "Dog" true)) (set! main_d (assoc main_d "pkg_DOG" true)) (set! main_d (assoc main_d "DOG" true)) (println (str (str "There are " (str (count main_d))) " dogs."))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (main))

(-main)
