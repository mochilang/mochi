(ns main (:refer-clojure :exclude [connect login changeDir list retrieve main]))

(require 'clojure.set)

(defrecord ServerNames [pub])

(defrecord ServerData [pub])

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(declare conn content data dataDir files names out serverData serverNames)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare connect login changeDir list retrieve main)

(def serverData {"pub" {"somefile.bin" "This is a file from the FTP server." "readme.txt" "Hello from ftp."}})

(def serverNames {"pub" ["somefile.bin" "readme.txt"]})

(defn connect [hostport]
  (try (do (println (str "Connected to " hostport)) (throw (ex-info "return" {:v {:dir "/"}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn login [conn user pass]
  (println (str "Logged in as " user)))

(defn changeDir [conn_p dir]
  (do (def conn conn_p) (def conn (assoc conn :dir dir))))

(defn list [conn]
  (try (do (def names (get serverNames (:dir conn))) (def dataDir (get serverData (:dir conn))) (def out []) (doseq [name names] (do (def content (nth dataDir name)) (def out (conj out {:name name :size (count content) :kind "file"})))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn retrieve [conn name]
  (try (throw (ex-info "return" {:v (get (get serverData (:dir conn)) name)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def conn (connect "localhost:21")) (login conn "anonymous" "anonymous") (changeDir conn "pub") (println (:dir conn)) (def files (list conn)) (doseq [f files] (println (str (str (:name f) " ") (str (:size f))))) (def data (retrieve conn "somefile.bin")) (println (str (str "Wrote " (str (count data))) " bytes to somefile.bin"))))

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
