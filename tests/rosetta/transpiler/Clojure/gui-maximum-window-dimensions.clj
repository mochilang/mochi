(ns main (:refer-clojure :exclude [maximize main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare maximize main)

(declare screen win)

(defn maximize [s win_p]
  (try (do (def win win_p) (def win (assoc win :w (:w s))) (def win (assoc win :h (:h s))) (def win (assoc win :maximized true)) (throw (ex-info "return" {:v win}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def screen {:w 1920 :h 1080}) (println (str (str (str "Screen size: " (str (:w screen))) " x ") (str (:h screen)))) (def win {:x 50 :y 50 :w 800 :h 600 :maximized false}) (def win (maximize screen win)) (println (str (str (str "Max usable : " (str (:w win))) " x ") (str (:h win))))))

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
