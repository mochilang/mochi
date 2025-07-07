(ns load-yaml
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-people [path]
  (->> (str/split-lines (slurp path))
       (partition 3)
       (map (fn [[n a e]]
              {:name (str/trim (subs n 7))
               :age (Integer/parseInt (str/trim (subs a 6)))
               :email (str/trim (subs e 8))}))))

(def people (parse-people "../interpreter/valid/people.yaml"))

(def adults (filter #(>= (:age %) 18) people))
(doseq [a adults]
  (println (:name a) (:email a)))
