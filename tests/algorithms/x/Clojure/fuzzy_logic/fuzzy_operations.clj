(ns main (:refer-clojure :exclude [stringify max2 min2 complement intersection union membership]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare stringify max2 min2 complement intersection union membership)

(defn stringify [stringify_fs]
  (try (throw (ex-info "return" {:v (str (str (str (str (str (str (str (:name stringify_fs) ": [") (str (:left_boundary stringify_fs))) ", ") (str (:peak stringify_fs))) ", ") (str (:right_boundary stringify_fs))) "]")})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn max2 [max2_a max2_b]
  (try (if (> max2_a max2_b) max2_a max2_b) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn min2 [min2_a min2_b]
  (try (if (< min2_a min2_b) min2_a min2_b) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn complement [complement_fs]
  (try (throw (ex-info "return" {:v {:left_boundary (- 1.0 (:right_boundary complement_fs)) :name (str "¬" (:name complement_fs)) :peak (- 1.0 (:left_boundary complement_fs)) :right_boundary (- 1.0 (:peak complement_fs))}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn intersection [intersection_a intersection_b]
  (try (throw (ex-info "return" {:v {:left_boundary (max2 (:left_boundary intersection_a) (:left_boundary intersection_b)) :name (str (str (:name intersection_a) " ∩ ") (:name intersection_b)) :peak (min2 (:right_boundary intersection_a) (:right_boundary intersection_b)) :right_boundary (/ (+ (:peak intersection_a) (:peak intersection_b)) 2.0)}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn union [union_a union_b]
  (try (throw (ex-info "return" {:v {:left_boundary (min2 (:left_boundary union_a) (:left_boundary union_b)) :name (str (str (:name union_a) " U ") (:name union_b)) :peak (max2 (:right_boundary union_a) (:right_boundary union_b)) :right_boundary (/ (+ (:peak union_a) (:peak union_b)) 2.0)}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn membership [membership_fs membership_x]
  (try (do (when (or (<= membership_x (:left_boundary membership_fs)) (>= membership_x (:right_boundary membership_fs))) (throw (ex-info "return" {:v 0.0}))) (when (and (< (:left_boundary membership_fs) membership_x) (<= membership_x (:peak membership_fs))) (throw (ex-info "return" {:v (quot (- membership_x (:left_boundary membership_fs)) (- (:peak membership_fs) (:left_boundary membership_fs)))}))) (if (and (< (:peak membership_fs) membership_x) (< membership_x (:right_boundary membership_fs))) (quot (- (:right_boundary membership_fs) membership_x) (- (:right_boundary membership_fs) (:peak membership_fs))) 0.0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_sheru {:left_boundary 0.4 :name "Sheru" :peak 1.0 :right_boundary 0.6})

(def ^:dynamic main_siya {:left_boundary 0.5 :name "Siya" :peak 1.0 :right_boundary 0.7})

(def ^:dynamic main_sheru_comp (complement main_sheru))

(def ^:dynamic main_inter (intersection main_siya main_sheru))

(def ^:dynamic main_uni (union main_siya main_sheru))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (stringify main_sheru))
      (println (stringify main_siya))
      (println (stringify main_sheru_comp))
      (println (stringify main_inter))
      (println (str "Sheru membership 0.5: " (str (membership main_sheru 0.5))))
      (println (str "Sheru membership 0.6: " (str (membership main_sheru 0.6))))
      (println (stringify main_uni))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
