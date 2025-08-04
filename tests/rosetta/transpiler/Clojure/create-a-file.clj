(ns main (:refer-clojure :exclude [createFile createDir main]))

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

(declare createFile createDir main)

(def ^:dynamic createDir_fs nil)

(def ^:dynamic createFile_fs nil)

(def ^:dynamic main_fs nil)

(defn createFile [createFile_fs_p createFile_fn]
  (binding [createFile_fs nil] (do (set! createFile_fs createFile_fs_p) (if (in createFile_fn createFile_fs) (println (str (str "open " createFile_fn) ": file exists")) (do (set! createFile_fs (assoc createFile_fs createFile_fn false)) (println (str (str "file " createFile_fn) " created!")))))))

(defn createDir [createDir_fs_p createDir_dn]
  (binding [createDir_fs nil] (do (set! createDir_fs createDir_fs_p) (if (in createDir_dn createDir_fs) (println (str (str "mkdir " createDir_dn) ": file exists")) (do (set! createDir_fs (assoc createDir_fs createDir_dn true)) (println (str (str "directory " createDir_dn) " created!")))))))

(defn main []
  (binding [main_fs nil] (do (set! main_fs {}) (set! main_fs (assoc main_fs "docs" true)) (createFile main_fs "input.txt") (createFile main_fs "/input.txt") (createDir main_fs "docs") (createDir main_fs "/docs"))))

(defn -main []
  (main))

(-main)
