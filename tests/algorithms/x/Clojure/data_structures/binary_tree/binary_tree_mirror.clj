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

(defn toi [s]
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare binary_tree_mirror_dict binary_tree_mirror main)

(declare _read_file)

(def ^:dynamic binary_tree_mirror_dict_children nil)

(def ^:dynamic binary_tree_mirror_dict_left nil)

(def ^:dynamic binary_tree_mirror_dict_right nil)

(def ^:dynamic binary_tree_mirror_dict_tree nil)

(def ^:dynamic binary_tree_mirror_k nil)

(def ^:dynamic binary_tree_mirror_tree_copy nil)

(def ^:dynamic main_binary_tree nil)

(def ^:dynamic main_mirrored nil)

(defn binary_tree_mirror_dict [binary_tree_mirror_dict_tree_p binary_tree_mirror_dict_root]
  (binding [binary_tree_mirror_dict_tree binary_tree_mirror_dict_tree_p binary_tree_mirror_dict_children nil binary_tree_mirror_dict_left nil binary_tree_mirror_dict_right nil] (try (do (when (or (= binary_tree_mirror_dict_root 0) (not (in binary_tree_mirror_dict_root binary_tree_mirror_dict_tree))) (throw (ex-info "return" {:v nil}))) (set! binary_tree_mirror_dict_children (get binary_tree_mirror_dict_tree binary_tree_mirror_dict_root)) (set! binary_tree_mirror_dict_left (get binary_tree_mirror_dict_children 0)) (set! binary_tree_mirror_dict_right (get binary_tree_mirror_dict_children 1)) (set! binary_tree_mirror_dict_tree (assoc binary_tree_mirror_dict_tree binary_tree_mirror_dict_root [binary_tree_mirror_dict_right binary_tree_mirror_dict_left])) (let [__res (binary_tree_mirror_dict binary_tree_mirror_dict_tree binary_tree_mirror_dict_left)] (do (set! binary_tree_mirror_dict_tree binary_tree_mirror_dict_tree) __res)) (let [__res (binary_tree_mirror_dict binary_tree_mirror_dict_tree binary_tree_mirror_dict_right)] (do (set! binary_tree_mirror_dict_tree binary_tree_mirror_dict_tree) __res))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var binary_tree_mirror_dict_tree) (constantly binary_tree_mirror_dict_tree))))))

(defn binary_tree_mirror [binary_tree_mirror_binary_tree binary_tree_mirror_root]
  (binding [binary_tree_mirror_k nil binary_tree_mirror_tree_copy nil] (try (do (when (= (count binary_tree_mirror_binary_tree) 0) (throw (Exception. "binary tree cannot be empty"))) (when (not (in binary_tree_mirror_root binary_tree_mirror_binary_tree)) (throw (Exception. (str (str "root " (mochi_str binary_tree_mirror_root)) " is not present in the binary_tree")))) (set! binary_tree_mirror_tree_copy {}) (doseq [binary_tree_mirror_k (keys binary_tree_mirror_binary_tree)] (set! binary_tree_mirror_tree_copy (assoc binary_tree_mirror_tree_copy binary_tree_mirror_k (get binary_tree_mirror_binary_tree binary_tree_mirror_k)))) (let [__res (binary_tree_mirror_dict binary_tree_mirror_tree_copy binary_tree_mirror_root)] (do (set! binary_tree_mirror_tree_copy binary_tree_mirror_dict_tree) __res)) (throw (ex-info "return" {:v binary_tree_mirror_tree_copy}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_binary_tree nil main_mirrored nil] (do (set! main_binary_tree {1 [2 3] 2 [4 5] 3 [6 7] 7 [8 9]}) (println (str "Binary tree: " (mochi_str main_binary_tree))) (set! main_mirrored (binary_tree_mirror main_binary_tree 1)) (println (str "Binary tree mirror: " (mochi_str main_mirrored))))))

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
