(ns main (:refer-clojure :exclude [cz]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare cz)

(declare cz_branch cz_sb cz_stem cz_y main_animal main_branchChArr main_element main_r main_stemChArr main_yinYang)

(def main_animal ["Rat" "Ox" "Tiger" "Rabbit" "Dragon" "Snake" "Horse" "Goat" "Monkey" "Rooster" "Dog" "Pig"])

(def main_yinYang ["Yang" "Yin"])

(def main_element ["Wood" "Fire" "Earth" "Metal" "Water"])

(def main_stemChArr ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])

(def main_branchChArr ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])

(defn cz [cz_yr cz_animal cz_yinYang cz_element cz_sc cz_bc]
  (try (do (def cz_y (- cz_yr 4)) (def cz_stem (mod cz_y 10)) (def cz_branch (mod cz_y 12)) (def cz_sb (str (nth cz_sc cz_stem) (nth cz_bc cz_branch))) (throw (ex-info "return" {:v {:animal (str (nth cz_animal cz_branch)) :yinYang (str (nth cz_yinYang (mod cz_stem 2))) :element (str (nth cz_element (int (/ cz_stem 2)))) :stemBranch cz_sb :cycle (+ (mod cz_y 60) 1)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (doseq [yr [1935 1938 1968 1972 1976]] (do (def main_r (cz yr main_animal main_yinYang main_element main_stemChArr main_branchChArr)) (println (str (str (str (str (str (str (str (str (str (str (str yr) ": ") (:element main_r)) " ") (:animal main_r)) ", ") (:yinYang main_r)) ", Cycle year ") (str (:cycle main_r))) " ") (:stemBranch main_r)))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
