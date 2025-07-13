(ns main)

(defn _min [v]
  (let [lst (cond
              (and (map? v) (contains? v :Items)) (:Items v)
              (sequential? v) v
              :else (throw (ex-info "min() expects list or group" {})))]
    (if (empty? lst)
      0
      (reduce (fn [a b] (if (neg? (compare a b)) a b)) lst))))

(defn _equal [a b]
  (cond
    (and (sequential? a) (sequential? b))
      (and (= (count a) (count b)) (every? true? (map _equal a b)))
    (and (map? a) (map? b))
      (and (= (count a) (count b))
           (every? (fn [k] (_equal (get a k) (get b k))) (keys a)))
    (and (number? a) (number? b))
      (= (double a) (double b))
    :else
      (= a b)))

(defn _escape_json [s]
  (-> s
      (clojure.string/replace "\\" "\\\\")
      (clojure.string/replace "\"" "\\\"")))

(defn _to_json [v]
  (cond
    (nil? v) "null"
    (string? v) (str "\"" (_escape_json v) "\"")
    (number? v) (str v)
    (boolean? v) (str v)
    (sequential? v) (str "[" (clojure.string/join "," (map _to_json v)) "]")
    (map? v) (str "{" (clojure.string/join "," (map (fn [[k val]]
                                        (str "\"" (_escape_json (name k)) "\":" (_to_json val))) v)) "}")
    :else (str "\"" (_escape_json (str v)) "\"")))

(defn _json [v]
  (println (_to_json v)))

(declare info_type keyword title movie_keyword movie_info_idx rows result)

(defn test_Q4_returns_minimum_rating_and_title_for_sequels []
  (assert (_equal result [{:rating "6.2" :movie_title "Alpha Movie"}]) "expect failed")
)

(defn -main []
  (def info_type [{:id 1 :info "rating"} {:id 2 :info "other"}]) ;; list of
  (def keyword [{:id 1 :keyword "great sequel"} {:id 2 :keyword "prequel"}]) ;; list of
  (def title [{:id 10 :title "Alpha Movie" :production_year 2006} {:id 20 :title "Beta Film" :production_year 2007} {:id 30 :title "Old Film" :production_year 2004}]) ;; list of
  (def movie_keyword [{:movie_id 10 :keyword_id 1} {:movie_id 20 :keyword_id 1} {:movie_id 30 :keyword_id 1}]) ;; list of
  (def movie_info_idx [{:movie_id 10 :info_type_id 1 :info "6.2"} {:movie_id 20 :info_type_id 1 :info "7.8"} {:movie_id 30 :info_type_id 1 :info "4.5"}]) ;; list of
  (def rows (vec (->> (for [it info_type mi movie_info_idx :when (_equal (:id it) (:info_type_id mi)) t title :when (_equal (:id t) (:movie_id mi)) mk movie_keyword :when (_equal (:movie_id mk) (:id t)) k keyword :when (_equal (:id k) (:keyword_id mk)) :when (and (and (and (and (_equal (:info it) "rating") (clojure.string/includes? (:keyword k) "sequel")) (> (compare (:info mi) "5.0") 0)) (> (:production_year t) 2005)) (_equal (:movie_id mk) (:movie_id mi)))] {:rating (:info mi) :title (:title t)})))) ;; list of
  (def result [{:rating (_min (vec (->> (for [r rows] (:rating r))))) :movie_title (_min (vec (->> (for [r rows] (:title r)))))}]) ;; list of
  (_json result)
  (test_Q4_returns_minimum_rating_and_title_for_sequels)
)

(-main)
