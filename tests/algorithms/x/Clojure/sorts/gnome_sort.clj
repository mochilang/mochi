(ns main (:refer-clojure :exclude [gnome_sort]))

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

(declare gnome_sort)

(def ^:dynamic gnome_sort_i nil)

(def ^:dynamic gnome_sort_lst nil)

(def ^:dynamic gnome_sort_tmp nil)

(defn gnome_sort [gnome_sort_lst_p]
  (binding [gnome_sort_i nil gnome_sort_lst nil gnome_sort_tmp nil] (try (do (set! gnome_sort_lst gnome_sort_lst_p) (when (<= (count gnome_sort_lst) 1) (throw (ex-info "return" {:v gnome_sort_lst}))) (set! gnome_sort_i 1) (while (< gnome_sort_i (count gnome_sort_lst)) (if (<= (nth gnome_sort_lst (- gnome_sort_i 1)) (nth gnome_sort_lst gnome_sort_i)) (set! gnome_sort_i (+ gnome_sort_i 1)) (do (set! gnome_sort_tmp (nth gnome_sort_lst (- gnome_sort_i 1))) (set! gnome_sort_lst (assoc gnome_sort_lst (- gnome_sort_i 1) (nth gnome_sort_lst gnome_sort_i))) (set! gnome_sort_lst (assoc gnome_sort_lst gnome_sort_i gnome_sort_tmp)) (set! gnome_sort_i (- gnome_sort_i 1)) (when (= gnome_sort_i 0) (set! gnome_sort_i 1))))) (throw (ex-info "return" {:v gnome_sort_lst}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (gnome_sort [0 5 3 2 2]))
      (println (gnome_sort []))
      (println (gnome_sort [(- 2) (- 5) (- 45)]))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
