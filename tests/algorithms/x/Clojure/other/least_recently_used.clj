(ns main (:refer-clojure :exclude [new_cache remove_element refer display repr_item cache_repr]))

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

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare new_cache remove_element refer display repr_item cache_repr)

(def ^:dynamic cache_repr_i nil)

(def ^:dynamic cache_repr_res nil)

(def ^:dynamic display_i nil)

(def ^:dynamic main_lru nil)

(def ^:dynamic new_cache_cap nil)

(def ^:dynamic refer_exists nil)

(def ^:dynamic refer_i nil)

(def ^:dynamic refer_j nil)

(def ^:dynamic refer_new_store nil)

(def ^:dynamic refer_store nil)

(def ^:dynamic remove_element_i nil)

(def ^:dynamic remove_element_removed nil)

(def ^:dynamic remove_element_res nil)

(def ^:dynamic remove_element_v nil)

(def ^:dynamic repr_item_all_digits nil)

(def ^:dynamic repr_item_ch nil)

(def ^:dynamic repr_item_i nil)

(defn new_cache [new_cache_n]
  (binding [new_cache_cap nil] (try (do (when (< new_cache_n 0) (throw (Exception. "n should be an integer greater than 0."))) (set! new_cache_cap (if (= new_cache_n 0) 2147483647 new_cache_n)) (throw (ex-info "return" {:v {:max_capacity new_cache_cap :store []}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn remove_element [remove_element_xs remove_element_x]
  (binding [remove_element_i nil remove_element_removed nil remove_element_res nil remove_element_v nil] (try (do (set! remove_element_res []) (set! remove_element_removed false) (set! remove_element_i 0) (while (< remove_element_i (count remove_element_xs)) (do (set! remove_element_v (nth remove_element_xs remove_element_i)) (if (and (= remove_element_removed false) (= remove_element_v remove_element_x)) (set! remove_element_removed true) (set! remove_element_res (vec (concat remove_element_res [remove_element_v])))) (set! remove_element_i (+ remove_element_i 1)))) (throw (ex-info "return" {:v remove_element_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn refer [refer_cache refer_x]
  (binding [refer_exists nil refer_i nil refer_j nil refer_new_store nil refer_store nil] (try (do (set! refer_store (:store refer_cache)) (set! refer_exists false) (set! refer_i 0) (while (< refer_i (count refer_store)) (do (when (= (get refer_store refer_i) refer_x) (set! refer_exists true)) (set! refer_i (+ refer_i 1)))) (if refer_exists (set! refer_store (remove_element refer_store refer_x)) (when (= (count refer_store) (:max_capacity refer_cache)) (do (set! refer_new_store []) (set! refer_j 0) (while (< refer_j (- (count refer_store) 1)) (do (set! refer_new_store (vec (concat refer_new_store [(get refer_store refer_j)]))) (set! refer_j (+ refer_j 1)))) (set! refer_store refer_new_store)))) (set! refer_store (vec (concat [refer_x] refer_store))) (throw (ex-info "return" {:v {:max_capacity (:max_capacity refer_cache) :store refer_store}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn display [display_cache]
  (binding [display_i nil] (do (set! display_i 0) (while (< display_i (count (:store display_cache))) (do (println (get (:store display_cache) display_i)) (set! display_i (+ display_i 1)))) display_cache)))

(defn repr_item [repr_item_s]
  (binding [repr_item_all_digits nil repr_item_ch nil repr_item_i nil] (try (do (set! repr_item_all_digits true) (set! repr_item_i 0) (while (< repr_item_i (count repr_item_s)) (do (set! repr_item_ch (subs repr_item_s repr_item_i (+ repr_item_i 1))) (when (or (< (compare repr_item_ch "0") 0) (> (compare repr_item_ch "9") 0)) (set! repr_item_all_digits false)) (set! repr_item_i (+ repr_item_i 1)))) (if repr_item_all_digits repr_item_s (str (str "'" repr_item_s) "'"))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn cache_repr [cache_repr_cache]
  (binding [cache_repr_i nil cache_repr_res nil] (try (do (set! cache_repr_res (str (str "LRUCache(" (str (:max_capacity cache_repr_cache))) ") => [")) (set! cache_repr_i 0) (while (< cache_repr_i (count (:store cache_repr_cache))) (do (set! cache_repr_res (str cache_repr_res (repr_item (get (:store cache_repr_cache) cache_repr_i)))) (when (< cache_repr_i (- (count (:store cache_repr_cache)) 1)) (set! cache_repr_res (str cache_repr_res ", "))) (set! cache_repr_i (+ cache_repr_i 1)))) (set! cache_repr_res (str cache_repr_res "]")) (throw (ex-info "return" {:v cache_repr_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_lru (new_cache 4))

(def ^:dynamic main_r (cache_repr main_lru))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (def main_lru (refer main_lru "A"))
      (def main_lru (refer main_lru "2"))
      (def main_lru (refer main_lru "3"))
      (def main_lru (refer main_lru "A"))
      (def main_lru (refer main_lru "4"))
      (def main_lru (refer main_lru "5"))
      (println main_r)
      (when (not= main_r "LRUCache(4) => [5, 4, 'A', 3]") (throw (Exception. "Assertion error")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
