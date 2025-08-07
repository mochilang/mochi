(ns main (:refer-clojure :exclude [ndvi bndvi gndvi ndre ccci cvi gli dvi calc main]))

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

(declare ndvi bndvi gndvi ndre ccci cvi gli dvi calc main)

(def ^:dynamic main_blue nil)

(def ^:dynamic main_green nil)

(def ^:dynamic main_nir nil)

(def ^:dynamic main_red nil)

(def ^:dynamic main_redEdge nil)

(defn ndvi [ndvi_red ndvi_nir]
  (try (throw (ex-info "return" {:v (quot (- ndvi_nir ndvi_red) (+ ndvi_nir ndvi_red))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bndvi [bndvi_blue bndvi_nir]
  (try (throw (ex-info "return" {:v (quot (- bndvi_nir bndvi_blue) (+ bndvi_nir bndvi_blue))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn gndvi [gndvi_green gndvi_nir]
  (try (throw (ex-info "return" {:v (quot (- gndvi_nir gndvi_green) (+ gndvi_nir gndvi_green))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn ndre [ndre_redEdge ndre_nir]
  (try (throw (ex-info "return" {:v (quot (- ndre_nir ndre_redEdge) (+ ndre_nir ndre_redEdge))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn ccci [ccci_red ccci_redEdge ccci_nir]
  (try (throw (ex-info "return" {:v (quot (ndre ccci_redEdge ccci_nir) (ndvi ccci_red ccci_nir))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn cvi [cvi_red cvi_green cvi_nir]
  (try (throw (ex-info "return" {:v (quot (* cvi_nir cvi_red) (* cvi_green cvi_green))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn gli [gli_red gli_green gli_blue]
  (try (throw (ex-info "return" {:v (quot (- (- (* 2.0 gli_green) gli_red) gli_blue) (+ (+ (* 2.0 gli_green) gli_red) gli_blue))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn dvi [dvi_red dvi_nir]
  (try (throw (ex-info "return" {:v (quot dvi_nir dvi_red)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn calc [calc_index calc_red calc_green calc_blue calc_redEdge calc_nir]
  (try (do (when (= calc_index "NDVI") (throw (ex-info "return" {:v (ndvi calc_red calc_nir)}))) (when (= calc_index "BNDVI") (throw (ex-info "return" {:v (bndvi calc_blue calc_nir)}))) (when (= calc_index "GNDVI") (throw (ex-info "return" {:v (gndvi calc_green calc_nir)}))) (when (= calc_index "NDRE") (throw (ex-info "return" {:v (ndre calc_redEdge calc_nir)}))) (when (= calc_index "CCCI") (throw (ex-info "return" {:v (ccci calc_red calc_redEdge calc_nir)}))) (when (= calc_index "CVI") (throw (ex-info "return" {:v (cvi calc_red calc_green calc_nir)}))) (when (= calc_index "GLI") (throw (ex-info "return" {:v (gli calc_red calc_green calc_blue)}))) (if (= calc_index "DVI") (dvi calc_red calc_nir) 0.0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (binding [main_blue nil main_green nil main_nir nil main_red nil main_redEdge nil] (do (set! main_red 50.0) (set! main_green 30.0) (set! main_blue 10.0) (set! main_redEdge 40.0) (set! main_nir 100.0) (println (str "NDVI=" (str (ndvi main_red main_nir)))) (println (str "CCCI=" (str (ccci main_red main_redEdge main_nir)))) (println (str "CVI=" (str (cvi main_red main_green main_nir)))))))

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
