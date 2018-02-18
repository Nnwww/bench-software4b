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
           [s4.t000002 TestCase2])
  (:gen-class))

(assert (-> (shell/sh "type" "git") :exit (= 0))  "Pre check error: not found git")
(assert (-> (shell/sh "type" "javac") :exit (= 0)) "Pre check error: not found javac")
(assert (-> (shell/sh "type" "java") :exit (= 0)) "Pre check error: not found java")

(def user-cd-path (System/getProperty "user.home"))

(defn get-canonical-path [^String path]
  {:pre [nil? path]}
  (cond-> path (.startsWith path "~") (.replaceFirst "~" user-cd-path)))

;; classpathは末尾バックスラッシュ必要
;; Fileオブジェクトは削ってしまうので注意
(def stab-compilable-user-path (io/file (get-canonical-path "~/WorkSpace/software_correct/tut2017informationQuantity/s4/b141837")))
(def stab-compilable-classpath (get-canonical-path "~/WorkSpace/software_correct/tut2017informationQuantity/"))

(def compile-names ["Frequencer" "InformationEstimator"])
(def test-names ["Minimal" "FullSpec"])
(def test-functions [[#(TestCase2/frequencerBasicTest %) #(TestCase2/frequencerSpecTest %)]
                     [#(TestCase2/informationEstimatorBasicTest %) #(TestCase2/informationEstimatorSpecTest %)]])
(def test-suite (reduce #(->> (map vector test-names (nth test-functions %2))
                              (assoc %1 (nth compile-names %2)))
                        {} (range (count compile-names))))

(def stab-env {:names compile-names :classpath stab-compilable-classpath})

(defn ret-assert [x message ret]
  (do (assert x message) ret))

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
    compile-print-strings))

(defn split-class-name-from-fqcn [fqcn] (last (cstr/split fqcn #"\.")))

(defn reflect-class-or-err-str [fqcn]
  (try (resolve (symbol fqcn))
       (catch ClassNotFoundException e
         (jansi-clj.core/yellow
          (str (split-class-name-from-fqcn fqcn) "(Class not found)")))))

(defn test-a-set [class test-kind test-set]
  (try (do (test-set class)
           (jansi-clj.core/green test-kind))
       (catch AssertionError e ;; テストケースに失敗がある場合
         (jansi-clj.core/red test-kind))
       (catch NullPointerException e ;; インターフェースを満たせてない場合
         (jansi-clj.core/red test-kind))))

(defn test-class [name class]
  (if (string? class) (cons class nil)
      (print-str (map #(apply (partial test-a-set class) %) (test-suite name)))))

(defn pairing-name-and-class [name fqcn]
  [name (reflect-class-or-err-str fqcn)])

(defn test-user [env user-path-obj]
  (let [package-name-prefix (str "s4." (.getName user-path-obj) ".")
        fqcns (map #(str package-name-prefix %) (:names env))]
    (->> (:names env)
         (#(map vector % fqcns))
         (map #(apply pairing-name-and-class %))
         (map #(vector (first %) (apply test-class %))))))

;;手前の関数をテストしてここから再開、TestSoftware的なjavaファイルを作り投げ込むコードをかく。
;; このためにはテストスイート公開用のjavaファイルの位置とか、来週までの隠し場所とかを奥本君と相談する必要がある
;; テストスイートは失敗時に例外を投げて貰うと良さそう

(defn unsnoc [str]
  (if (empty? str) str
      (.substring str 0 (dec (.length str)))))

(defn last-char [str]
  (if (empty? str) str
      (.charAt str (dec (.length str)))))

(defn init-work-dir [path]
  (if (not (nil? path))
    (if (= \/ (last-char path)) path
        (str path "/"))
    (let [result-git-root (git-root-path user-cd-path)]
      (if (zero? (:exit result-git-root))
        (str (:out result-git-root) "/")
        (throw (Exception. (str "Illegal Work Directory Error: This program works in git directories. "
                                "Please cd or specify a git project by args.")))))))

(defn evaluate-user [env user-path-obj]
  (do (println (.getName user-path-obj))
      (println (str "build: " (apply print-str (build-user env user-path-obj))))
      (println (str "tests: " (apply print-str (test-user env user-path-obj))))
      (println (str "bench: " ))
      (print "\n")))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (jansi-clj.core/green  "The green string indicates success."))
  (println (jansi-clj.core/red    "The red string indicates failure."))
  (println (jansi-clj.core/yellow "The yellow string indicates the absence of class files. Please consult with TA."))
  (print "\n")
  (let [classpath (init-work-dir (first args))]
    (doall
     (->> (str classpath "s4")
          io/file
          file-seq
          extract-students-folder
          (map #(evaluate-user
                 {:names compile-names :classpath classpath} %))))))
