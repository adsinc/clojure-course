(ns dsl.core
  (:use clojure.walk)
  (:import (java.util Date Calendar)))

(def cal (Calendar/getInstance))
(def today (Date.))
(def yesterday (do (.add cal Calendar/DATE -1) (.getTime cal)))
(def tomorrow (do (.add cal Calendar/DATE 2) (.getTime cal)))

(comment
  (defn one [] 1)

  ;; Примеры вызова
  (with-datetime
    (if (> today tomorrow) (println "Time goes wrong"))
    (if (<= yesterday today) (println "Correct"))
    (let [six (+ 1 2 3)
          d1 (today - 2 days)
          d2 (today + 1 week)
          d3 (today + six months)
          d4 (today + (one) year)]
      (if (and (< d1 d2)
               (< d2 d3)
               (< d3 d4))
        (println "DSL works correctly")))))


;; Поддерживаемые операции:
;; > >= < <=
;; Функция принимает на вход три аргумента. Она должна определить,
;; являются ли второй и третий аргумент датами. Если являются,
;; то из дат необходимо взять date.getTime и сравнить их по этому числу.
;; Если получены не даты, то выполнить операцию op в обычном порядке:
;; (op d1 d2).
(defn d-op [op d1 d2]
  (if (and (instance? Date d1) (instance? Date d2))
    (op (.getTime d1) (.getTime d2))
    (op d1 d2)))

;; Пример вызова:
;; (d-add today '+ 1 'day)
;; Функция должна на основе своих параметров создать новую дату.
;; Дата создается при помощи календаря, например так:
;; (def cal (java.util.Calendar/getInstance))
;; (.add cal java.util.Calendar/DATE 2)
;; (.getTime cal)
;; Во-первых, необходимо на основе 'op' и 'num' определить количество, на
;; которое будем изменять дату. 'Op' может принимать + и -, соответственно
;; нужно будет не изменять либо изменить знак числа 'num'.
;; Во-вторых, необходимо узнать период, на который будем изменять дату.
;; Например, если получили 'day, то в вызове (.add cal ...) будем использовать
;; java.util.Calendar/DATE. Если получили 'months, то java.util.Calendar/MONTH.
;; И так далее.
;; Результат работы функции - новая дата, получаемая из календаря так: (.getTime cal)
(defn d-add [date op num period]
  (let [delta (eval `(~op ~num))
        remove-s (fn [str]
                   (if (.endsWith str "s")
                     (subs str 0 (dec (.length str)))
                     str))
        cal-period (->> period
                        str
                        remove-s
                        (#(get {"day" "date"
                                "week" "week_of_year"} % %))
                        .toUpperCase
                        (symbol "java.util.Calendar")
                        eval)]
    (.getTime
      (doto (Calendar/getInstance)
        (.setTime date)
        (.add cal-period delta)))))

;; Можете использовать эту функцию для того, чтобы определить,
;; является ли список из 4-х элементов тем самым списком, который создает новую дату,
;; и который нужно обработать функцией d-add.
(defn is-date-op? [code]
  (let [op (second code)
        period (last code)]
    (and (= (count code) 4)
         (or (= '+ op)
             (= '- op))
         (contains? #{'day 'days 'week 'weeks 'month 'months 'year 'years
                      'hour 'hours 'minute 'minutes 'second 'seconds} period))))

(defn is-d-op? [code]
  (let [op (first code)]
    (and (= (count code) 3)
         (or (= '> op)
             (= '>= op)
             (=  '< op)
             (= '<= op)))))

(defn replace-dsl
  [form]
  (if (coll? form)
    (cond
      (is-date-op? form)
      `(d-add ~@(drop-last 1 form) '~@(take-last 1 form))
      (is-d-op? form) `(d-op ~@form)
      :else ((if (vector? form) mapv map)
              replace-dsl
              form))
    form))

;; В code содержится код-как-данные. Т.е. сам code -- коллекция, но его содержимое --
;; нормальный код на языке Clojure.
;; Нам необходимо пройтись по каждому элементу этого кода, найти все списки из 3-х элементов,
;; в которых выполняется сравнение, и подставить вместо этого кода вызов d-op;
;; а для списков из четырех элементов, в которых создаются даты, подставить функцию d-add.
(defmacro with-datetime [& code]
  (let [new-code (replace-dsl code)]
  `(do ~@new-code)))