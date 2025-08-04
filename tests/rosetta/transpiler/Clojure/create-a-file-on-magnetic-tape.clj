(ns main (:refer-clojure :exclude [gzipWriter tarWriter tarWriteHeader tarWrite main]))

(require 'clojure.set)

(defrecord Hdr [Name Mode Size ModTime Typeflag Uname Gname])

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(def ^:dynamic main_data nil)

(def ^:dynamic main_filename nil)

(def ^:dynamic main_gzipFlag nil)

(def ^:dynamic main_hdr nil)

(def ^:dynamic main_outfile nil)

(def ^:dynamic main_w nil)

(declare gzipWriter tarWriter tarWriteHeader tarWrite main)

(defn gzipWriter [gzipWriter_w]
  (try (throw (ex-info "return" {:v gzipWriter_w})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn tarWriter [tarWriter_w]
  (try (throw (ex-info "return" {:v tarWriter_w})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn tarWriteHeader [tarWriteHeader_w tarWriteHeader_hdr]
  (do))

(defn tarWrite [tarWrite_w tarWrite_data]
  (do))

(defn main []
  (binding [main_data nil main_filename nil main_gzipFlag nil main_hdr nil main_outfile nil main_w nil] (do (set! main_filename "TAPE.FILE") (set! main_data "") (set! main_outfile "") (set! main_gzipFlag false) (set! main_w nil) (when (not= main_outfile "") (set! main_w nil)) (when main_gzipFlag (set! main_w (gzipWriter main_w))) (set! main_w (tarWriter main_w)) (set! main_hdr {"Name" main_filename "Mode" 432 "Size" (count main_data) "ModTime" (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647))) "Typeflag" 0 "Uname" "guest" "Gname" "guest"}) (tarWriteHeader main_w main_hdr) (tarWrite main_w main_data))))

(defn -main []
  (main))

(-main)
