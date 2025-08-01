(ns main (:refer-clojure :exclude [main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare main)

(declare main_DOG main_Dog main_d main_dog main_pkg_DOG main_pkg_dog)

(defn packageSees [packageSees_d1 packageSees_d2 packageSees_d3]
  (try (do (println (str (str (str (str (str "Package sees: " packageSees_d1) " ") packageSees_d2) " ") packageSees_d3)) (throw (ex-info "return" {:v {"pkg_dog" true "Dog" true "pkg_DOG" true}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (try (do (def main_pkg_dog "Salt") (def main_Dog "Pepper") (def main_pkg_DOG "Mustard") (def main_d (packageSees main_pkg_dog main_Dog main_pkg_DOG)) (println (str (str "There are " (str (count main_d))) " dogs.\n")) (def main_dog "Benjamin") (def main_d (packageSees main_pkg_dog main_Dog main_pkg_DOG)) (println (str (str (str (str (str "Main sees:   " main_dog) " ") main_Dog) " ") main_pkg_DOG)) (def main_d (assoc main_d "dog" true)) (def main_d (assoc main_d "Dog" true)) (def main_d (assoc main_d "pkg_DOG" true)) (println (str (str "There are " (str (count main_d))) " dogs.\n")) (def main_Dog "Samba") (def main_d (packageSees main_pkg_dog main_Dog main_pkg_DOG)) (println (str (str (str (str (str "Main sees:   " main_dog) " ") main_Dog) " ") main_pkg_DOG)) (def main_d (assoc main_d "dog" true)) (def main_d (assoc main_d "Dog" true)) (def main_d (assoc main_d "pkg_DOG" true)) (println (str (str "There are " (str (count main_d))) " dogs.\n")) (def main_DOG "Bernie") (def main_d (packageSees main_pkg_dog main_Dog main_pkg_DOG)) (println (str (str (str (str (str "Main sees:   " main_dog) " ") main_Dog) " ") main_DOG)) (def main_d (assoc main_d "dog" true)) (def main_d (assoc main_d "Dog" true)) (def main_d (assoc main_d "pkg_DOG" true)) (def main_d (assoc main_d "DOG" true)) (println (str (str "There are " (str (count main_d))) " dogs."))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (main)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
