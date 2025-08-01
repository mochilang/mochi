(ns main (:refer-clojure :exclude [showState maximize unmaximize iconify deiconify hide showWindow move main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare showState maximize unmaximize iconify deiconify hide showWindow move main)

(declare win)

(defn showState [w label]
  (println (str (str (str (str (str (str (str (str (str (str (str (str (str (str label ": pos=(") (str (:x w))) ",") (str (:y w))) ") size=(") (str (:w w))) "x") (str (:h w))) ") max=") (str (:maximized w))) " icon=") (str (:iconified w))) " visible=") (str (:visible w)))))

(defn maximize [w_p]
  (try (do (def w w_p) (def w (assoc w :maximized true)) (def w (assoc w :w 800)) (def w (assoc w :h 600)) (throw (ex-info "return" {:v w}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn unmaximize [w_p]
  (try (do (def w w_p) (def w (assoc w :maximized false)) (def w (assoc w :w 640)) (def w (assoc w :h 480)) (throw (ex-info "return" {:v w}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn iconify [w_p]
  (try (do (def w w_p) (def w (assoc w :iconified true)) (def w (assoc w :visible false)) (throw (ex-info "return" {:v w}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn deiconify [w_p]
  (try (do (def w w_p) (def w (assoc w :iconified false)) (def w (assoc w :visible true)) (throw (ex-info "return" {:v w}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn hide [w_p]
  (try (do (def w w_p) (def w (assoc w :visible false)) (throw (ex-info "return" {:v w}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn showWindow [w_p]
  (try (do (def w w_p) (def w (assoc w :visible true)) (throw (ex-info "return" {:v w}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn move [w_p]
  (try (do (def w w_p) (if (:shifted w) (do (def w (assoc w :x (- (:x w) 10))) (def w (assoc w :y (- (:y w) 10)))) (do (def w (assoc w :x (+' (:x w) 10))) (def w (assoc w :y (+' (:y w) 10))))) (def w (assoc w :shifted (not (:shifted w)))) (throw (ex-info "return" {:v w}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def win {:x 100 :y 100 :w 640 :h 480 :maximized false :iconified false :visible true :shifted false}) (showState win "Start") (def win (maximize win)) (showState win "Maximize") (def win (unmaximize win)) (showState win "Unmaximize") (def win (iconify win)) (showState win "Iconify") (def win (deiconify win)) (showState win "Deiconify") (def win (hide win)) (showState win "Hide") (def win (showWindow win)) (showState win "Show") (def win (move win)) (showState win "Move")))

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
