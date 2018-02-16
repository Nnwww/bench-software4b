(ns bench-software4b.core
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.pprint :as pp]
            [clojure.string :as cstr]
            [clojure.spec.alpha :as s]
            [jansi-clj.core])
  (:import [java.nio.file.Files]
           [java.nio.file.Paths]
           [java.io.File]
           [java.lang.ReflectiveOperationException])
  (:gen-class))

(assert (-> (shell/sh "type" "git") :exit (= 0))  "Pre check error: not found git")
(assert (-> (shell/sh "type" "javac") :exit (= 0)) "Pre check error: not found javac")
(assert (-> (shell/sh "type" "java") :exit (= 0)) "Pre check error: not found java")

(defn ret-assert [x message ret]
  (do (assert x message) ret))

(def user-home-path (System/getProperty "user.home"))

(s/def ::shell-opt? #{:in :in-enc :out-enc :env :dir})
(s/def ::args-type-cmd
  (s/cat :cmd-arg (s/+ string?)
         :opt (s/* (s/cat :opt-key ::shell-opt?
                          :opt-val string?))))
(s/fdef type-cmd :args ::args-type-cmd)

(defn type-cmd [& args]
  (let [{[cmd & evaluted-str] :cmd-arg opts :opt} (s/conform ::args-type-cmd args)]
    (do (assert (-> (shell/sh "type" cmd) :exit (= 0))  "Error: not found commnad")
        (let [result-cmd (apply shell/sh cmd evaluted-str)]
          (ret-assert (-> result-cmd :exit (= 0)) (str "Error: " (:err result-cmd)) (:out result-cmd))))))

(defn git-root-path [exec-dir]
  (-> (if (nil? exec-dir)
        (shell/sh "git" "rev-parse" "--show-toplevel")
        (shell/sh "git" "rev-parse" "--show-toplevel" :dir exec-dir))
      (update :out cstr/trim-newline)))

(defn cmd-javac [user-path-obj compile-name classpath]
  (let [user-path (.getAbsolutePath user-path-obj)
        compile-target (str user-path "/" compile-name ".java")]
    (shell/sh "javac" compile-target "-classpath" classpath :dir user-path)))

(defn cmd-java [compile-path]
  (shell/sh "java" compile-path "-classpath" "."))

(defn get-canonical-path [^String path]
  {:pre [nil? path]}
  (cond-> path (.startsWith path "~") (.replaceFirst "~" user-home-path)))

(defn extract-students-folder [files]
  (filter #(and (.isDirectory %)
                (->> % .getName (re-find #"b([1-9])+"))) files))

(defn diff-relative-paths [target-file decrease-file]
  (-> target-file
      .toURI
      (.relativize (.toURI decrease-file))
      .toString
      io/file))

(defn show-build-result [compile-name sh-result]
  (if (= 0 (:exit sh-result))
    (jansi-clj.core/green compile-name)
    (jansi-clj.core/red compile-name)))

(defn pairing-name-and-result [env user-path-obj compile-name]
  [compile-name
   (cmd-javac user-path-obj compile-name (:classpath env))])

(defn build-user [env user-path-obj]
  (let [compile-results (map #(pairing-name-and-result env user-path-obj %) (:names env))
        compile-print-strings (map #(apply show-build-result %) compile-results)]
    (println
     (reduce #(str %1 " " %2)
             (str (.getName user-path-obj) ":")
             compile-print-strings))))

(defn split-class-name-from-fqcn [fqcn] (last (cstr/split fqcn #"\.")))

(defn reflect-class-or-err-str [fqcn]
  (try (resolve (symbol fqcn))
       (catch ClassNotFoundException e
         (jansi-clj.core/yellow
          (str (split-class-name-from-fqcn fqcn) "(Class not found)")))))

(defn test-class [fqcn]
  "Instantiate from the FQCN name.
  This function return a testing result with reflected instance or an error string."
  (let [class-or-str (reflect-class-or-err-str fqcn)]
      (if (string? class-or-str)
        class-or-str
        ( class-or-str))))

(defn test-user [env user-path-obj]
  (let [package-name-prefix (str "s4." (.getName user-path-obj) ".")]
    (->> (:names env)
         (map #(str package-name-prefix %))
         (map #(reflect-class-or-err-str %))
         (map #(test-class %)))))
;;手前の関数をテストしてここから再開、TestSoftware的なjavaファイルを作り投げ込むコードをかく。
;; このためにはテストスイート公開用のjavaファイルの位置とか、来週までの隠し場所とかを奥本君と相談する必要がある
;; テストスイートは失敗時に例外を投げて貰うと良さそう

;; classpathは末尾バックスラッシュ必要
;; Fileオブジェクトは削ってしまうので注意
(def stab-compilable-user-path (io/file (get-canonical-path "~/WorkSpace/tut2017informationQuantity/s4/b141837")))
(def stab-compilable-classpath (get-canonical-path "~/WorkSpace/tut2017informationQuantity/"))
(def compile-names ["Frequencer" "InformationEstimator" "TestCase"])

(defn unsnoc [str]
  (if (empty? str) str
      (.substring str 0 (dec (.length str)))))

(defn last-char [str]
  (if (empty? str) str
      (.charAt str (dec (.length str)))))

(defn init-work-params [args]
  (if (not (zero? (count args)))
    (let [adir (first args)]
      (if (= \/ (last-char (adir))) adir
          (str adir "/")))
    (let [result-git-root (git-root-path (System/getProperty "user.dir"))]
      (if (zero? (:exit result-git-root))
        (str (:out result-git-root) "/")
        (throw (Exception. (str "Illegal Work Directory Error: This program works in git directories."
                                "Please cd or specify a project root by args.")))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (do
    (let [classpath (init-work-params args)]
      (->> (str classpath "s4")
           io/file
           file-seq
           extract-students-folder
           (map #(build-user {:names compile-names :classpath stab-compilable-classpath} %))))
           ;; 完成時にstabをclasspathにおきかえる
    0))
