(ns main (:refer-clojure :exclude [beastKind beastName beastCry bprint main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare beastKind beastName beastCry bprint main)

(defn beastKind [b]
  (try (throw (ex-info "return" {:v (cond (and (map? b) (= (:__tag b) "Dog") (contains? b :kind) (contains? b :name)) (let [k (:kind b) _ (:name b)] k) (and (map? b) (= (:__tag b) "Cat") (contains? b :kind) (contains? b :name)) (let [k (:kind b) _ (:name b)] k))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn beastName [b]
  (try (throw (ex-info "return" {:v (cond (and (map? b) (= (:__tag b) "Dog") (contains? b :kind) (contains? b :name)) (let [_ (:kind b) n (:name b)] n) (and (map? b) (= (:__tag b) "Cat") (contains? b :kind) (contains? b :name)) (let [_ (:kind b) n (:name b)] n))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn beastCry [b]
  (try (throw (ex-info "return" {:v (cond (and (map? b) (= (:__tag b) "Dog") (contains? b :kind) (contains? b :name)) (let [_ (:kind b) _ (:name b)] "Woof") (and (map? b) (= (:__tag b) "Cat") (contains? b :kind) (contains? b :name)) (let [_ (:kind b) _ (:name b)] "Meow"))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bprint [b]
  (println (str (str (str (str (str (beastName b) ", who's a ") (beastKind b)) ", cries: \"") (beastCry b)) "\".")))

(defn main []
  (do (def d {:__tag "Dog" :kind "labrador" :name "Max"}) (def c {:__tag "Cat" :kind "siamese" :name "Sammy"}) (bprint d) (bprint c)))

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
