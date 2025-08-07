(ns main (:refer-clojure :exclude [binary_tree_mirror_dict binary_tree_mirror main]))

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

(declare binary_tree_mirror_dict binary_tree_mirror main)

(def ^:dynamic binary_tree_mirror_dict_children nil)

(def ^:dynamic binary_tree_mirror_dict_left nil)

(def ^:dynamic binary_tree_mirror_dict_right nil)

(def ^:dynamic binary_tree_mirror_dict_tree nil)

(def ^:dynamic binary_tree_mirror_tree_copy nil)

(def ^:dynamic main_binary_tree nil)

(def ^:dynamic main_mirrored nil)

(defn binary_tree_mirror_dict [binary_tree_mirror_dict_tree_p binary_tree_mirror_dict_root]
  (binding [binary_tree_mirror_dict_children nil binary_tree_mirror_dict_left nil binary_tree_mirror_dict_right nil binary_tree_mirror_dict_tree nil] (try (do (set! binary_tree_mirror_dict_tree binary_tree_mirror_dict_tree_p) (when (or (= binary_tree_mirror_dict_root 0) (not (in binary_tree_mirror_dict_root binary_tree_mirror_dict_tree))) (throw (ex-info "return" {:v nil}))) (set! binary_tree_mirror_dict_children (nth binary_tree_mirror_dict_tree binary_tree_mirror_dict_root)) (set! binary_tree_mirror_dict_left (nth binary_tree_mirror_dict_children 0)) (set! binary_tree_mirror_dict_right (nth binary_tree_mirror_dict_children 1)) (set! binary_tree_mirror_dict_tree (assoc binary_tree_mirror_dict_tree binary_tree_mirror_dict_root [binary_tree_mirror_dict_right binary_tree_mirror_dict_left])) (binary_tree_mirror_dict binary_tree_mirror_dict_tree binary_tree_mirror_dict_left) (binary_tree_mirror_dict binary_tree_mirror_dict_tree binary_tree_mirror_dict_right)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn binary_tree_mirror [binary_tree_mirror_binary_tree binary_tree_mirror_root]
  (binding [binary_tree_mirror_tree_copy nil] (try (do (when (= (count binary_tree_mirror_binary_tree) 0) (throw (Exception. "binary tree cannot be empty"))) (when (not (in binary_tree_mirror_root binary_tree_mirror_binary_tree)) (throw (Exception. (str (str "root " (str binary_tree_mirror_root)) " is not present in the binary_tree")))) (set! binary_tree_mirror_tree_copy {}) (doseq [k binary_tree_mirror_binary_tree] (set! binary_tree_mirror_tree_copy (assoc binary_tree_mirror_tree_copy k (nth binary_tree_mirror_binary_tree k)))) (binary_tree_mirror_dict binary_tree_mirror_tree_copy binary_tree_mirror_root) (throw (ex-info "return" {:v binary_tree_mirror_tree_copy}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_binary_tree nil main_mirrored nil] (do (set! main_binary_tree {1 [2 3] 2 [4 5] 3 [6 7] 7 [8 9]}) (println (str "Binary tree: " (str main_binary_tree))) (set! main_mirrored (binary_tree_mirror main_binary_tree 1)) (println (str "Binary tree mirror: " (str main_mirrored))))))

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
