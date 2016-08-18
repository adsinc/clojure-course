(ns task02.query
  (:use [task02 helpers db]
        [clojure.core.match :only (match)]))

;; Функция выполняющая парсинг запроса переданного пользователем
;;
;; Синтаксис запроса:
;; SELECT table_name [WHERE column comp-op value] [ORDER BY column] [LIMIT N] [JOIN other_table ON left_column = right_column]
;;
;; - Имена колонок указываются в запросе как обычные имена, а не в виде keywords. В
;;   результате необходимо преобразовать в keywords
;; - Поддерживаемые операторы WHERE: =, !=, <, >, <=, >=
;; - Имя таблицы в JOIN указывается в виде строки - оно будет передано функции get-table для получения объекта
;; - Значение value может быть либо числом, либо строкой в одинарных кавычках ('test').
;;   Необходимо вернуть данные соответствующего типа, удалив одинарные кавычки для строки.
;;
;; - Ключевые слова --> case-insensitive
;;
;; Функция должна вернуть последовательность со следующей структурой:
;;  - имя таблицы в виде строки
;;  - остальные параметры которые будут переданы select
;;
;; Если запрос нельзя распарсить, то вернуть nil

;; Примеры вызова:
;; > (parse-select "select student")
;; ("student")
;; > (parse-select "select student where id = 10")
;; ("student" :where #<function>)
;; > (parse-select "select student where id = 10 limit 2")
;; ("student" :where #<function> :limit 2)
;; > (parse-select "select student where id = 10 order by id limit 2")
;; ("student" :where #<function> :order-by :id :limit 2)
;; > (parse-select "select student where id = 10 order by id limit 2 join subject on id = sid")
;; ("student" :where #<function> :order-by :id :limit 2 :joins [[:id "subject" :sid]])
;; > (parse-select "werfwefw")
;; nil
(declare parse-select-vec)
(declare make-where-function)

(defn parse-select
  [^String sel-string]
  (let [select-vec (vec (.split sel-string " "))]
        (parse-select-vec select-vec)))

(defn parse-select-vec
  ([sel-vec] (parse-select-vec sel-vec [] []))
  ([sel-vec acc join-acc]
   (let [append (fn [& xs] (apply conj acc xs))
         rec (fn [parsed-count & elems]
               (parse-select-vec
                 (subvec sel-vec parsed-count)
                 (apply append elems)
                 join-acc))]
     (match sel-vec
            ["select" tbl-name & _]
            (rec 2 tbl-name)
            ["where" left op right & _]
            (rec 4 :where (make-where-function left op right))
            ["limit" num & _]
            (rec 2 :limit (parse-int num))
            ["order" "by" column & _]
            (rec 3 :order-by (keyword column))
            ["join" tbl-name "on" left "=" right & other]
            (parse-select-vec other acc (conj join-acc [(keyword left) tbl-name (keyword right)]))
            :else (if (empty? sel-vec)
                    (concat acc [:joins join-acc])
                    nil)))))

(defn make-where-function [& args]
  (let [[col f-name num] args
        fn-sym (symbol f-name)
        compare-fn (resolve fn-sym)]
    (fn [rec]
      (compare-fn (col rec) num))))

;; Выполняет запрос переданный в строке.  Бросает исключение если не удалось распарсить запрос

;; Примеры вызова:
;; > (perform-query "select student")
;; ({:id 1, :year 1998, :surname "Ivanov"} {:id 2, :year 1997, :surname "Petrov"} {:id 3, :year 1996, :surname "Sidorov"})
;; > (perform-query "select student order by year")
;; ({:id 3, :year 1996, :surname "Sidorov"} {:id 2, :year 1997, :surname "Petrov"} {:id 1, :year 1998, :surname "Ivanov"})
;; > (perform-query "select student where id > 1")
;; ({:id 2, :year 1997, :surname "Petrov"} {:id 3, :year 1996, :surname "Sidorov"})
;; > (perform-query "not valid")
;; exception...
(defn perform-query [^String sel-string]
  (if-let [query (parse-select sel-string)]
    (apply select (get-table (first query)) (rest query))
    (throw (IllegalArgumentException. (str "Can't parse query: " sel-string)))))
