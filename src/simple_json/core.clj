(ns simple-json.core
  (:require [clojure.string :as cls]))

(defn -main
  "I don't do a whole lot."
  []
  (println "Hello, World!"))
(def my-str "{\"name\":\"John\", \"age\":null, \"city\":\"New York\" \"salary\": 500.55 \"exponential\": 2E20}")
(def regex-strings
  {
   :int #"([\+-]?\d+)([eE][\+-]?\d+)?"                  ; #"/^([+-]?[1-9]\\d*|0)$/"
   :double #"/^[0-9]+(\\.[0-9]+)?$"
   :exponential #"e e+ e- E E+ E-"
   }
  )
(defn get-object
  "Gets the string content inside {} structure"
  [json-string]
  (if (empty? json-string)
    nil
    (let [output-object (subs json-string (inc (cls/index-of json-string "{"))  (cls/last-index-of json-string "}"))]
      (if (empty? output-object)
        nil
        output-object)
      )
    )
  )
(defn split-object
  "Splits the object on ',' "
  [object-string]
  (map clojure.string/trim (cls/split object-string #","))
  )
(defn parse-string
  [input-str]
  (if (= \" (first input-str))
    (let [x (subs input-str 1 (cls/index-of input-str "\"" 2))]
      (if x
        [x (subs input-str (+ 2 (count x)))]
        [nil input-str])
      )
    [nil input-str]
    )
  )
(defn parse-null
  "Parses null values to nil"
  [input-str]
  (if (= "null" (subs input-str 0 4))
    [nil (subs input-str 4)]
    input-str)
  )
(defn parse-space
  "Removes whitespaces from left of strings"
  [input-str]
  (if (= \space (first input-str))
    (cls/triml input-str)
    input-str)
  )
(defn parse-comma
  ""
  [input-str]
  (if (= \, (first input-str))
    (subs input-str 1)
    input-str)
  )

(defn parse-boolean
  "Parses the string value to bool value"
  [input-str]
    (cond
      (= (subs input-str 0 4) "true") [true (subs input-str 4)]
      (= (subs input-str 0 5) "false") [false (subs input-str 5)]
      :else [nil input-str]
      )
  )
(defn parse-int
  "Parses the string to int"
  [input-str]
  (let [x (re-find (:int regex-strings) input-str)]
    (if x
      [(Integer/parseInt x) (subs input-str (count input-str))]
      [nil input-str]
      )
    )
  )
(defn parse-double
  "Parses double value in string"
  [input-str]
  (let [x (re-find (:double regex-strings) input-str)]
    (if x
      [(Double/parseDouble x) (subs input-str (count input-str))]
      [nil input-str]
      )
    )
  )
(defn parse-colon
  "A value or object or array can be encountered after a colon"
  [input-str]
  (if (= \: (first input-str))
    ;parse a value complete this method
    [nil (subs input-str 1)]
    )
  )
(defn parse-literal
  "Parses null true false"
  [input-str]
  (let [[x xs] (parse-boolean input-str)]
    (if x [x xs]
          (let [[y ys] (parse-null input-str)]
            [y ys])
          )
    )
  )
(defn parse-number
  "Parses a string to it's semantic data type
  Todo implement for exponential
  "
  [input-str]
  (let [[x y] (parse-int input-str)]
    (if x [x y]
          (let [[a b] (parse-double input-str)]
            (if a [a b]
                  [a b])
            )
          )
    )
  )




(println (get-object my-str))
(println (clojure.string/split (get-object my-str) #","))
(println (split-object (get-object my-str)))
(println (parse-string "\"hello\" baby"))
;(println (parse-int "45"))