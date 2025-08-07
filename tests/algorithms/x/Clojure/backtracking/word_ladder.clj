(ns main (:refer-clojure :exclude [contains remove_item word_ladder main]))

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

(declare contains remove_item word_ladder main)

(def ^:dynamic contains_i nil)

(def ^:dynamic main_w1 nil)

(def ^:dynamic main_w2 nil)

(def ^:dynamic main_w3 nil)

(def ^:dynamic main_w4 nil)

(def ^:dynamic remove_item_i nil)

(def ^:dynamic remove_item_removed nil)

(def ^:dynamic remove_item_res nil)

(def ^:dynamic word_ladder_c nil)

(def ^:dynamic word_ladder_i nil)

(def ^:dynamic word_ladder_j nil)

(def ^:dynamic word_ladder_new_path nil)

(def ^:dynamic word_ladder_new_words nil)

(def ^:dynamic word_ladder_result nil)

(def ^:dynamic word_ladder_transformed nil)

(def ^:dynamic main_alphabet "abcdefghijklmnopqrstuvwxyz")

(defn contains [contains_xs contains_x]
  (binding [contains_i nil] (try (do (set! contains_i 0) (while (< contains_i (count contains_xs)) (do (when (= (nth contains_xs contains_i) contains_x) (throw (ex-info "return" {:v true}))) (set! contains_i (+ contains_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn remove_item [remove_item_xs remove_item_x]
  (binding [remove_item_i nil remove_item_removed nil remove_item_res nil] (try (do (set! remove_item_res []) (set! remove_item_removed false) (set! remove_item_i 0) (while (< remove_item_i (count remove_item_xs)) (do (if (and (not remove_item_removed) (= (nth remove_item_xs remove_item_i) remove_item_x)) (set! remove_item_removed true) (set! remove_item_res (conj remove_item_res (nth remove_item_xs remove_item_i)))) (set! remove_item_i (+ remove_item_i 1)))) (throw (ex-info "return" {:v remove_item_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn word_ladder [word_ladder_current word_ladder_path word_ladder_target word_ladder_words]
  (binding [word_ladder_c nil word_ladder_i nil word_ladder_j nil word_ladder_new_path nil word_ladder_new_words nil word_ladder_result nil word_ladder_transformed nil] (try (do (when (= word_ladder_current word_ladder_target) (throw (ex-info "return" {:v word_ladder_path}))) (set! word_ladder_i 0) (while (< word_ladder_i (count word_ladder_current)) (do (set! word_ladder_j 0) (while (< word_ladder_j (count main_alphabet)) (do (set! word_ladder_c (subs main_alphabet word_ladder_j (min (+ word_ladder_j 1) (count main_alphabet)))) (set! word_ladder_transformed (str (str (subs word_ladder_current 0 (min word_ladder_i (count word_ladder_current))) word_ladder_c) (subs word_ladder_current (+ word_ladder_i 1) (min (count word_ladder_current) (count word_ladder_current))))) (when (contains word_ladder_words word_ladder_transformed) (do (set! word_ladder_new_words (remove_item word_ladder_words word_ladder_transformed)) (set! word_ladder_new_path (conj word_ladder_path word_ladder_transformed)) (set! word_ladder_result (word_ladder word_ladder_transformed word_ladder_new_path word_ladder_target word_ladder_new_words)) (when (> (count word_ladder_result) 0) (throw (ex-info "return" {:v word_ladder_result}))))) (set! word_ladder_j (+ word_ladder_j 1)))) (set! word_ladder_i (+ word_ladder_i 1)))) (throw (ex-info "return" {:v []}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_w1 nil main_w2 nil main_w3 nil main_w4 nil] (do (set! main_w1 ["hot" "dot" "dog" "lot" "log" "cog"]) (println (str (word_ladder "hit" ["hit"] "cog" main_w1))) (set! main_w2 ["hot" "dot" "dog" "lot" "log"]) (println (str (word_ladder "hit" ["hit"] "cog" main_w2))) (set! main_w3 ["load" "goad" "gold" "lead" "lord"]) (println (str (word_ladder "lead" ["lead"] "gold" main_w3))) (set! main_w4 ["came" "cage" "code" "cade" "gave"]) (println (str (word_ladder "game" ["game"] "code" main_w4))))))

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
