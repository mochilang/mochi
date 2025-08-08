(ns main (:refer-clojure :exclude [uf_make uf_find uf_union boruvka main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(defn toi [s]
  (Integer/parseInt (str s)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare uf_make uf_find uf_union boruvka main)

(def ^:dynamic boruvka_cheap nil)

(def ^:dynamic boruvka_e nil)

(def ^:dynamic boruvka_fr1 nil)

(def ^:dynamic boruvka_fr2 nil)

(def ^:dynamic boruvka_i nil)

(def ^:dynamic boruvka_idx nil)

(def ^:dynamic boruvka_idxe nil)

(def ^:dynamic boruvka_mst nil)

(def ^:dynamic boruvka_num_components nil)

(def ^:dynamic boruvka_set1 nil)

(def ^:dynamic boruvka_set2 nil)

(def ^:dynamic boruvka_uf nil)

(def ^:dynamic boruvka_v nil)

(def ^:dynamic main_edges nil)

(def ^:dynamic main_mst nil)

(def ^:dynamic uf_find_p nil)

(def ^:dynamic uf_find_res nil)

(def ^:dynamic uf_make_i nil)

(def ^:dynamic uf_make_p nil)

(def ^:dynamic uf_make_r nil)

(def ^:dynamic uf_union_fr1 nil)

(def ^:dynamic uf_union_fr2 nil)

(def ^:dynamic uf_union_p nil)

(def ^:dynamic uf_union_r nil)

(def ^:dynamic uf_union_root1 nil)

(def ^:dynamic uf_union_root2 nil)

(def ^:dynamic uf_union_uf1 nil)

(defn uf_make [uf_make_n]
  (binding [uf_make_i nil uf_make_p nil uf_make_r nil] (try (do (set! uf_make_p []) (set! uf_make_r []) (set! uf_make_i 0) (while (< uf_make_i uf_make_n) (do (set! uf_make_p (conj uf_make_p uf_make_i)) (set! uf_make_r (conj uf_make_r 0)) (set! uf_make_i (+ uf_make_i 1)))) (throw (ex-info "return" {:v {:parent uf_make_p :rank uf_make_r}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn uf_find [uf_find_uf uf_find_x]
  (binding [uf_find_p nil uf_find_res nil] (try (do (set! uf_find_p (:parent uf_find_uf)) (when (not= (get uf_find_p uf_find_x) uf_find_x) (do (set! uf_find_res (uf_find {:parent uf_find_p :rank (:rank uf_find_uf)} (get uf_find_p uf_find_x))) (set! uf_find_p (:parent (:uf uf_find_res))) (set! uf_find_p (assoc uf_find_p uf_find_x (:root uf_find_res))) (throw (ex-info "return" {:v {:root (:root uf_find_res) :uf {:parent uf_find_p :rank (:rank (:uf uf_find_res))}}})))) (throw (ex-info "return" {:v {:root uf_find_x :uf uf_find_uf}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn uf_union [uf_union_uf uf_union_x uf_union_y]
  (binding [uf_union_fr1 nil uf_union_fr2 nil uf_union_p nil uf_union_r nil uf_union_root1 nil uf_union_root2 nil uf_union_uf1 nil] (try (do (set! uf_union_fr1 (uf_find uf_union_uf uf_union_x)) (set! uf_union_uf1 (:uf uf_union_fr1)) (set! uf_union_root1 (:root uf_union_fr1)) (set! uf_union_fr2 (uf_find uf_union_uf1 uf_union_y)) (set! uf_union_uf1 (:uf uf_union_fr2)) (set! uf_union_root2 (:root uf_union_fr2)) (when (= uf_union_root1 uf_union_root2) (throw (ex-info "return" {:v uf_union_uf1}))) (set! uf_union_p (:parent uf_union_uf1)) (set! uf_union_r (:rank uf_union_uf1)) (if (> (get uf_union_r uf_union_root1) (get uf_union_r uf_union_root2)) (set! uf_union_p (assoc uf_union_p uf_union_root2 uf_union_root1)) (if (< (get uf_union_r uf_union_root1) (get uf_union_r uf_union_root2)) (set! uf_union_p (assoc uf_union_p uf_union_root1 uf_union_root2)) (do (set! uf_union_p (assoc uf_union_p uf_union_root2 uf_union_root1)) (set! uf_union_r (assoc uf_union_r uf_union_root1 (+ (get uf_union_r uf_union_root1) 1)))))) (throw (ex-info "return" {:v {:parent uf_union_p :rank uf_union_r}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn boruvka [boruvka_n boruvka_edges]
  (binding [boruvka_cheap nil boruvka_e nil boruvka_fr1 nil boruvka_fr2 nil boruvka_i nil boruvka_idx nil boruvka_idxe nil boruvka_mst nil boruvka_num_components nil boruvka_set1 nil boruvka_set2 nil boruvka_uf nil boruvka_v nil] (try (do (set! boruvka_uf (uf_make boruvka_n)) (set! boruvka_num_components boruvka_n) (set! boruvka_mst []) (while (> boruvka_num_components 1) (do (set! boruvka_cheap []) (set! boruvka_i 0) (while (< boruvka_i boruvka_n) (do (set! boruvka_cheap (conj boruvka_cheap (- 0 1))) (set! boruvka_i (+ boruvka_i 1)))) (set! boruvka_idx 0) (while (< boruvka_idx (count boruvka_edges)) (do (set! boruvka_e (nth boruvka_edges boruvka_idx)) (set! boruvka_fr1 (uf_find boruvka_uf (:u boruvka_e))) (set! boruvka_uf (:uf boruvka_fr1)) (set! boruvka_set1 (:root boruvka_fr1)) (set! boruvka_fr2 (uf_find boruvka_uf (:v boruvka_e))) (set! boruvka_uf (:uf boruvka_fr2)) (set! boruvka_set2 (:root boruvka_fr2)) (when (not= boruvka_set1 boruvka_set2) (do (when (or (= (nth boruvka_cheap boruvka_set1) (- 0 1)) (> (:w (nth boruvka_edges (nth boruvka_cheap boruvka_set1))) (:w boruvka_e))) (set! boruvka_cheap (assoc boruvka_cheap boruvka_set1 boruvka_idx))) (when (or (= (nth boruvka_cheap boruvka_set2) (- 0 1)) (> (:w (nth boruvka_edges (nth boruvka_cheap boruvka_set2))) (:w boruvka_e))) (set! boruvka_cheap (assoc boruvka_cheap boruvka_set2 boruvka_idx))))) (set! boruvka_idx (+ boruvka_idx 1)))) (set! boruvka_v 0) (while (< boruvka_v boruvka_n) (do (set! boruvka_idxe (nth boruvka_cheap boruvka_v)) (when (not= boruvka_idxe (- 0 1)) (do (set! boruvka_e (nth boruvka_edges boruvka_idxe)) (set! boruvka_fr1 (uf_find boruvka_uf (:u boruvka_e))) (set! boruvka_uf (:uf boruvka_fr1)) (set! boruvka_set1 (:root boruvka_fr1)) (set! boruvka_fr2 (uf_find boruvka_uf (:v boruvka_e))) (set! boruvka_uf (:uf boruvka_fr2)) (set! boruvka_set2 (:root boruvka_fr2)) (when (not= boruvka_set1 boruvka_set2) (do (set! boruvka_mst (conj boruvka_mst boruvka_e)) (set! boruvka_uf (uf_union boruvka_uf boruvka_set1 boruvka_set2)) (set! boruvka_num_components (- boruvka_num_components 1)))))) (set! boruvka_v (+ boruvka_v 1)))))) (throw (ex-info "return" {:v boruvka_mst}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_edges nil main_mst nil] (do (set! main_edges [{:u 0 :v 1 :w 1} {:u 0 :v 2 :w 2} {:u 2 :v 3 :w 3}]) (set! main_mst (boruvka 4 main_edges)) (doseq [e main_mst] (println (str (str (str (str (str (:u e)) " - ") (str (:v e))) " : ") (str (:w e))))))))

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
