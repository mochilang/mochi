(ns main (:refer-clojure :exclude [make_set find_set union_set same_python_set]))

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

(declare make_set find_set union_set same_python_set)

(def ^:dynamic find_set_p nil)

(def ^:dynamic find_set_res nil)

(def ^:dynamic main_ds nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_j nil)

(def ^:dynamic make_set_p nil)

(def ^:dynamic make_set_r nil)

(def ^:dynamic union_set_ds1 nil)

(def ^:dynamic union_set_ds2 nil)

(def ^:dynamic union_set_fx nil)

(def ^:dynamic union_set_fy nil)

(def ^:dynamic union_set_p nil)

(def ^:dynamic union_set_r nil)

(def ^:dynamic union_set_x_root nil)

(def ^:dynamic union_set_y_root nil)

(defn make_set [make_set_ds make_set_x]
  (binding [make_set_p nil make_set_r nil] (try (do (set! make_set_p (:parent make_set_ds)) (set! make_set_r (:rank make_set_ds)) (set! make_set_p (assoc make_set_p make_set_x make_set_x)) (set! make_set_r (assoc make_set_r make_set_x 0)) (throw (ex-info "return" {:v {:parent make_set_p :rank make_set_r}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_set [find_set_ds find_set_x]
  (binding [find_set_p nil find_set_res nil] (try (do (when (= (get (:parent find_set_ds) find_set_x) find_set_x) (throw (ex-info "return" {:v {:ds find_set_ds :root find_set_x}}))) (set! find_set_res (find_set find_set_ds (get (:parent find_set_ds) find_set_x))) (set! find_set_p (:parent (:ds find_set_res))) (set! find_set_p (assoc find_set_p find_set_x (:root find_set_res))) (throw (ex-info "return" {:v {:ds {:parent find_set_p :rank (:rank (:ds find_set_res))} :root (:root find_set_res)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn union_set [union_set_ds union_set_x union_set_y]
  (binding [union_set_ds1 nil union_set_ds2 nil union_set_fx nil union_set_fy nil union_set_p nil union_set_r nil union_set_x_root nil union_set_y_root nil] (try (do (set! union_set_fx (find_set union_set_ds union_set_x)) (set! union_set_ds1 (:ds union_set_fx)) (set! union_set_x_root (:root union_set_fx)) (set! union_set_fy (find_set union_set_ds1 union_set_y)) (set! union_set_ds2 (:ds union_set_fy)) (set! union_set_y_root (:root union_set_fy)) (when (= union_set_x_root union_set_y_root) (throw (ex-info "return" {:v union_set_ds2}))) (set! union_set_p (:parent union_set_ds2)) (set! union_set_r (:rank union_set_ds2)) (if (> (nth union_set_r union_set_x_root) (nth union_set_r union_set_y_root)) (set! union_set_p (assoc union_set_p union_set_y_root union_set_x_root)) (do (set! union_set_p (assoc union_set_p union_set_x_root union_set_y_root)) (when (= (nth union_set_r union_set_x_root) (nth union_set_r union_set_y_root)) (set! union_set_r (assoc union_set_r union_set_y_root (+ (nth union_set_r union_set_y_root) 1)))))) (throw (ex-info "return" {:v {:parent union_set_p :rank union_set_r}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn same_python_set [same_python_set_a same_python_set_b]
  (try (do (when (and (< same_python_set_a 3) (< same_python_set_b 3)) (throw (ex-info "return" {:v true}))) (if (and (and (and (>= same_python_set_a 3) (< same_python_set_a 6)) (>= same_python_set_b 3)) (< same_python_set_b 6)) true false)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_ds {:parent [] :rank []})

(def ^:dynamic main_i 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (< main_i 6) (do (def main_ds (assoc main_ds :parent (conj (:parent main_ds) 0))) (def main_ds (assoc main_ds :rank (conj (:rank main_ds) 0))) (def main_ds (make_set main_ds main_i)) (def main_i (+ main_i 1))))
      (def main_ds (union_set main_ds 0 1))
      (def main_ds (union_set main_ds 1 2))
      (def main_ds (union_set main_ds 3 4))
      (def main_ds (union_set main_ds 3 5))
      (def main_i 0)
      (while (< main_i 6) (do (def ^:dynamic main_j 0) (while (< main_j 6) (do (def ^:dynamic main_res_i (find_set main_ds main_i)) (def main_ds (:ds main_res_i)) (def ^:dynamic main_root_i (:root main_res_i)) (def ^:dynamic main_res_j (find_set main_ds main_j)) (def main_ds (:ds main_res_j)) (def ^:dynamic main_root_j (:root main_res_j)) (def ^:dynamic main_same (same_python_set main_i main_j)) (def ^:dynamic main_root_same (= main_root_i main_root_j)) (if main_same (when (not main_root_same) (throw (Exception. "nodes should be in same set"))) (when main_root_same (throw (Exception. "nodes should be in different sets")))) (def main_j (+ main_j 1)))) (def main_i (+ main_i 1))))
      (def main_i 0)
      (while (< main_i 6) (do (def ^:dynamic main_res (find_set main_ds main_i)) (def main_ds (:ds main_res)) (println (str (:root main_res))) (def main_i (+ main_i 1))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
