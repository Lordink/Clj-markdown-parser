(ns clj-markdown-parser.core
  (:require [clojure.test :refer [deftest is run-tests]]
            [clojure.string :as str])
  (:gen-class))

(defn -main [& args]
  (println "OwO"))

(def simple-line "Line without any data")
(def line-with-bold "Line with **bold** in it")
(def line-with-link "line with [a Link](http://google.com) in it")
(def two-bold-lines "A line with [link](http://microsoft.com) in it\nAnd another line, also with some [href](http://google.com)")

(defn strip-word "Remove square brackets and parens" [line]
  (-> line
    (str/replace "[" "")
    (str/replace "]" "")
    (str/replace "(" "")
    (str/replace ")" "")))

(defn parse [md-str]
  (let [words             (str/split md-str #" ")
        output            (atom [])
        current-wip-token (atom nil)]
    (doseq [word words]
      (cond
        ;; It's a link, [alt](url)
        (str/starts-with? word "[") (do

                                      (swap! output conj :blyat))
        :else (swap! output conj :text word)))
    @output))

(deftest simple-text-parse
  (->
   simple-line
   parse
   (= [:text simple-line])
   is))

(deftest bold-line-parse
  (is (= (parse line-with-bold) [:text "Line with "
                                 :bold "bold"
                                 :text " in it"])))

(deftest link-line-parse
  (is (= (parse line-with-link) [:text "line with "
                                 :link {:alt "a Link"
                                        :url "http://google.com"}
                                 :text " in it"])))

(deftest link-line-parse-2
  (is (= (parse two-bold-lines) [:text "A line with "
                                 :link {:alt "href"
                                        :url "http://microsoft.com"}
                                 :text "\nAnd another line, also with some"
                                 :link {:alt "href"
                                        :url "http://google.com"}])))


(run-tests 'clj-markdown-parser.core)
(comment
  (parse line-with-bold)
  (parse line-with-link)
  (re-seq #"\w+" "stuff and things")
  (->> "stuff and things" (split-with #(not= " " %)) flatten)
  (str/split line-with-bold #"\s+")
  (str "one thing, " "another thing" ", third thing"))
