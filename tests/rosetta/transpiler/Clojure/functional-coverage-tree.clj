(ns main (:refer-clojure :exclude [pow10 formatFloat padLeft repeat toFloat newNode addChildren setCoverage computeCoverage spaces show main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare pow10 formatFloat padLeft repeat toFloat newNode addChildren setCoverage computeCoverage spaces show main)

(declare c cleaning cs diff digits fracPart h1_attic h1_basement h1_bathroom1 h1_bathroom2 h1_bathrooms h1_bedrooms h1_conservatory h1_dining h1_garage h1_garden h1_kitchen h1_living_rooms h1_lounge h1_outside h1_playroom h2_attics h2_basement h2_bathroom h2_bedroom3 h2_bedroom4 h2_bedrooms h2_cellars h2_cinema h2_conservatory h2_dining h2_garage h2_garden h2_groundfloor h2_hot_tub h2_kitchen h2_living_rooms h2_lounge h2_playroom h2_suite1 h2_suite2 h2_toilet h2_upstairs h2_wet_room h2_wine_cellar house1 house2 i indent intPart line m n name nl r res s scale scaled topCoverage v1 v2)

(defn pow10 [n]
  (try (do (def r 1.0) (def i 0) (while (< i n) (do (def r (* r 10.0)) (def i (+' i 1)))) (throw (ex-info "return" {:v r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn formatFloat [f prec]
  (try (do (def scale (pow10 prec)) (def scaled (+' (* f scale) 0.5)) (def n (int scaled)) (def digits (str n)) (while (<= (count digits) prec) (def digits (str "0" digits))) (def intPart (subs digits 0 (- (count digits) prec))) (def fracPart (subs digits (- (count digits) prec) (count digits))) (throw (ex-info "return" {:v (str (str intPart ".") fracPart)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn padLeft [s w]
  (try (do (def res "") (def n (- w (count s))) (while (> n 0) (do (def res (str res " ")) (def n (- n 1)))) (throw (ex-info "return" {:v (str res s)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn repeat [ch n]
  (try (do (def s "") (def i 0) (while (< i n) (do (def s (str s ch)) (def i (+' i 1)))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn toFloat [i]
  (try (throw (ex-info "return" {:v (double i)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn newNode [name weight coverage]
  (try (throw (ex-info "return" {:v {"name" name "weight" weight "coverage" coverage "children" []}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn addChildren [n_p nodes]
  (do (def n n_p) (def cs (get n "children")) (doseq [node nodes] (def cs (conj cs node))) (def n (assoc n "children" cs))))

(defn setCoverage [n_p value]
  (do (def n n_p) (def n (assoc n "coverage" value))))

(defn computeCoverage [n]
  (try (do (def cs (get n "children")) (when (= (count cs) 0) (throw (ex-info "return" {:v (double (get n "coverage"))}))) (def v1 0.0) (def v2 0) (doseq [node cs] (do (def m node) (def c (computeCoverage m)) (def v1 (+' v1 (* (toFloat (int (get m "weight"))) c))) (def v2 (+' v2 (int (get m "weight")))))) (throw (ex-info "return" {:v (/ v1 (toFloat v2))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn spaces [n]
  (try (throw (ex-info "return" {:v (repeat " " n)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn show [n level]
  (do (def indent (* level 4)) (def name (str (get n "name"))) (def nl (+' (count name) indent)) (def line (str (spaces indent) name)) (def line (str (str line (spaces (- 32 nl))) "|  ")) (def line (str (str line (padLeft (str (int (get n "weight"))) 3)) "   | ")) (def line (str (str line (formatFloat (computeCoverage n) 6)) " |")) (println line) (def cs (get n "children")) (doseq [child cs] (show child (+' level 1)))))

(def house1 (newNode "house1" 40 0.0))

(def house2 (newNode "house2" 60 0.0))

(def h1_bedrooms (newNode "bedrooms" 1 0.25))

(def h1_bathrooms (newNode "bathrooms" 1 0.0))

(def h1_attic (newNode "attic" 1 0.75))

(def h1_kitchen (newNode "kitchen" 1 0.1))

(def h1_living_rooms (newNode "living_rooms" 1 0.0))

(def h1_basement (newNode "basement" 1 0.0))

(def h1_garage (newNode "garage" 1 0.0))

(def h1_garden (newNode "garden" 1 0.8))

(def h2_upstairs (newNode "upstairs" 1 0.0))

(def h2_groundfloor (newNode "groundfloor" 1 0.0))

(def h2_basement (newNode "basement" 1 0.0))

(def h1_bathroom1 (newNode "bathroom1" 1 0.5))

(def h1_bathroom2 (newNode "bathroom2" 1 0.0))

(def h1_outside (newNode "outside_lavatory" 1 1.0))

(def h1_lounge (newNode "lounge" 1 0.0))

(def h1_dining (newNode "dining_room" 1 0.0))

(def h1_conservatory (newNode "conservatory" 1 0.0))

(def h1_playroom (newNode "playroom" 1 1.0))

(def h2_bedrooms (newNode "bedrooms" 1 0.0))

(def h2_bathroom (newNode "bathroom" 1 0.0))

(def h2_toilet (newNode "toilet" 1 0.0))

(def h2_attics (newNode "attics" 1 0.6))

(def h2_kitchen (newNode "kitchen" 1 0.0))

(def h2_living_rooms (newNode "living_rooms" 1 0.0))

(def h2_wet_room (newNode "wet_room_&_toilet" 1 0.0))

(def h2_garage (newNode "garage" 1 0.0))

(def h2_garden (newNode "garden" 1 0.9))

(def h2_hot_tub (newNode "hot_tub_suite" 1 1.0))

(def h2_cellars (newNode "cellars" 1 1.0))

(def h2_wine_cellar (newNode "wine_cellar" 1 1.0))

(def h2_cinema (newNode "cinema" 1 0.75))

(def h2_suite1 (newNode "suite_1" 1 0.0))

(def h2_suite2 (newNode "suite_2" 1 0.0))

(def h2_bedroom3 (newNode "bedroom_3" 1 0.0))

(def h2_bedroom4 (newNode "bedroom_4" 1 0.0))

(def h2_lounge (newNode "lounge" 1 0.0))

(def h2_dining (newNode "dining_room" 1 0.0))

(def h2_conservatory (newNode "conservatory" 1 0.0))

(def h2_playroom (newNode "playroom" 1 0.0))

(defn main []
  (do (def cleaning (newNode "cleaning" 1 0.0)) (addChildren h1_bathrooms [h1_bathroom1 h1_bathroom2 h1_outside]) (addChildren h1_living_rooms [h1_lounge h1_dining h1_conservatory h1_playroom]) (addChildren house1 [h1_bedrooms h1_bathrooms h1_attic h1_kitchen h1_living_rooms h1_basement h1_garage h1_garden]) (addChildren h2_bedrooms [h2_suite1 h2_suite2 h2_bedroom3 h2_bedroom4]) (addChildren h2_upstairs [h2_bedrooms h2_bathroom h2_toilet h2_attics]) (addChildren h2_living_rooms [h2_lounge h2_dining h2_conservatory h2_playroom]) (addChildren h2_groundfloor [h2_kitchen h2_living_rooms h2_wet_room h2_garage h2_garden h2_hot_tub]) (addChildren h2_basement [h2_cellars h2_wine_cellar h2_cinema]) (addChildren house2 [h2_upstairs h2_groundfloor h2_basement]) (addChildren cleaning [house1 house2]) (def topCoverage (computeCoverage cleaning)) (println (str "TOP COVERAGE = " (formatFloat topCoverage 6))) (println "") (println "NAME HIERARCHY                 | WEIGHT | COVERAGE |") (show cleaning 0) (setCoverage h2_cinema 1.0) (def diff (- (computeCoverage cleaning) topCoverage)) (println "") (println "If the coverage of the Cinema node were increased from 0.75 to 1") (println (str (str (str "the top level coverage would increase by " (formatFloat diff 6)) " to ") (formatFloat (+' topCoverage diff) 6))) (setCoverage h2_cinema 0.75)))

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
