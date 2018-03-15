(ns utils.core
    (:require [clj-time.core   :as t]
              [clj-time.format :as f]
              [clj-time.local  :as l]
			  [expound.alpha   :as ex]
              [clojure.java.io :as io]
			  [clojure.spec.alpha   :as s]
              [taoensso.timbre :as log]
              [clojure.string  :as str])
    (:import (java.awt.font TextLayout)
             (java.awt 		Color Font FontMetrics GraphicsEnvironment)))

;;-----------------------------------------------------------------------------
;; validation
;;-----------------------------------------------------------------------------

(defmacro valid?
	[sp v]
	`(if-not (s/valid? ~sp ~v)
		(log/error
			(str "\n---------- " ~*file* " " ~(:line (meta &form)) " ------------\n"
				 ~v
				 "\n---------------------------------------\n"
				 (ex/expound-str ~sp ~v)
				 "\n---------------------------------------\n"))
		true))

(defmacro validate
	[sp v]
	`(if-not (s/valid? ~sp ~v)
		(throw (ex-info (str ~*file* ":" ~(:line (meta &form)) " " (ex/expound-str ~sp ~v))
               			{:file ~*file* :line ~(:line (meta &form))}))
		~v))

;;-----------------------------------------------------------------------------
;; spying
;;-----------------------------------------------------------------------------

(defn spy
	([x]
	(prn "SPY:" (type x) x)
	x)
	([t x]
	(prn "SPY:" t x)
	x))

(defn spyf
  	([f x]
   	(prn "SPYF:" (f x))
    x)
  	([s f x]
   	(prn "SPYF:" s (f x))
    x))

;;-----------------------------------------------------------------------------
;; numbers
;;-----------------------------------------------------------------------------

(defn neg
    [value]
    (cond
      	(decimal? value) (- 0M value)
      	(float? value)   (- 0.0 value)
      	(integer? value) (- 0 value)
        :else (throw (Exception. (str "utils/neg: " value " is of unknown type " (type value))))))

(defn half
	[x]
	(/ x 2))

(defn avg
	[coll]
	(if (empty? coll)
		0
		(int (/ (apply + coll) (count coll)))))

(defn abs
	[x]
	(if (pos? x) x (neg x)))

(defn parse-int [s]
    (when-not (str/blank? s)
        (Long. (re-find  #"^-?\d+$" (str/trim s)))))

(defn parse-dec [s]
    ;(println "parse-dec:" s (re-find  #"^-?\d+(\.\d+)?M?$" s))
    (when-not (or (str/blank? s) (nil? (re-find  #"^-?\d+(\.\d+)?M?$" (str/trim s))))
        (Double. (first (re-find  #"^-?\d+(\.\d+)?M?$" (str/trim s))))))

(defn parse-bool [s]
    (= (str/lower-case (str/trim s)) "true"))

;;-----------------------------------------------------------------------------
;; strings
;;-----------------------------------------------------------------------------

(defn string-width
	"calculate width of string (in pixels)"
	[^java.awt.Graphics2D g2d txt-style txt]
	(if (and (some? g2d) (some? txt-style) (some? txt) (some? (:font txt-style)))
		(.stringWidth (.getFontMetrics g2d (:font txt-style)) txt)
		(do
			(prn "string-width:" g2d txt-style txt)
			200)))

(defn string-height
	"calculate height of string (in pixels)"
	[^java.awt.Graphics2D g2d txt-style txt]
	(-> (TextLayout. txt
									 (:font txt-style)
									 (.getFontRenderContext g2d))
			(.getOutline nil)
			(.getBounds)
			(.height)))

;;-----------------------------------------------------------------------------
;; system
;;-----------------------------------------------------------------------------

(defn get-screens
	[]
	(let [ge     (GraphicsEnvironment/getLocalGraphicsEnvironment)
				sd     (.getScreenDevices ge)
				bounds (fn [x] (.getBounds (.getDefaultConfiguration x)))]
		(map #(hash-map :x      (.x (bounds %))
					 :y      (.y (bounds %))
					 :width  (.width (bounds %))
					 :height (.height (bounds %)))
				 sd)))

(defn get-dir-list
	"read a directory"
	[dir re]
	(filter #(re-find re %) (map str (file-seq (io/file dir)))))

(defn retry-func
	[retries wait-sec func & opts]
	(try
		(apply func opts)
		(catch Exception e
			(if (zero? retries)
				(throw e)
				(do
					(Thread/sleep (* wait-sec 1000))
					(apply retry-func (dec retries) wait-sec func opts))))))

;;-----------------------------------------------------------------------------
;; collections
;;-----------------------------------------------------------------------------

(defn drop-nth
	[n coll]
	{:pre [(pos? n) (coll? coll)]
	 :post [(coll? %)]}
	(vec (keep-indexed #(when (not= %1 n) %2) coll)))

(defn find-first
	[f coll]
	(some (fn [x] (when (f x) x)) coll))

(defn b->n
	[x]
	(when (seq x) x))

(defn update-deep
	[func x]
	(cond
		(map? x)
		(into {} (for [[k v] (seq (func x))]
							 [k (update-deep func v)]))
		(set? x)
		(set (map #(update-deep func %) x))
		(coll? x)
		(mapv #(update-deep func %) x)
		:else x))

;;-----------------------------------------------------------------------------
;; date time
;;-----------------------------------------------------------------------------

(defn day-of-week
	[]
	(dec (t/day-of-week (l/local-now))))

(defn current-year
	[]
	(long (t/year (l/local-now))))

(defn current-month
	[]
	(long (t/month (l/local-now))))

(defn current-quarter
	[]
	(nth [0 1 1 1 2 2 2 3 3 3 4 4 4] (current-month)))

(defn now-str
    []
    (f/unparse (f/with-zone (f/formatters :mysql) (t/default-time-zone)) (l/local-now)))

(defn today-str
    []
    (f/unparse (f/with-zone (f/formatters :date) (t/default-time-zone)) (l/local-now)))

(defn year-month
    [dt]
    (f/unparse (f/with-zone (f/formatters :year-month) (t/default-time-zone)) dt))

(defn hour-minute
	([]
	 (hour-minute (l/local-now)))
	([dt]
	 (f/unparse (f/with-zone (f/formatters :hour-minute) (t/default-time-zone)) dt)))

(defn date-str
	"return current dat as a string"
	[]
	(f/unparse (f/with-zone (f/formatter "EEE yy-MM-dd") (t/default-time-zone)) (l/local-now)))

(defn time-str
	"return current time as a string"
	[]
	(f/unparse (f/with-zone (f/formatters :hour-minute-second) (t/default-time-zone)) (l/local-now)))

(defn within?
	([from to year]
	 {:pre [(valid? #(instance? org.joda.time.DateTime %) from)
					(valid? (s/nilable #(instance? org.joda.time.DateTime %)) to)
					(valid? integer? year)]
		:post [(valid? boolean? %)]}
	 (let [fromto     (t/interval from (or to (l/local-now)))
				 whole-year (t/interval (t/date-time year 1 1) (t/date-time year 12 31))]
		 (some? (t/overlap fromto whole-year))))
	([from to year month day]
	 {:pre [(valid? #(instance? org.joda.time.DateTime %) from)
					(valid? (s/nilable #(instance? org.joda.time.DateTime %)) to)
					(valid? integer? year)
					(valid? (s/int-in 1 13) month)
					(valid? (s/int-in 1 32) day)]
		:post [(valid? boolean? %)]}
	 (let [fromto (t/interval from (or to (l/local-now)))]
		 (t/within? fromto (t/date-time year month day)))))

(defn ft-within?
	([ft year]
	 (within? (:from ft) (:to ft) year))
	([ft year month]
	 (within? (:from ft) (:to ft) year month 15)))

;;-----------------------------------------------------------------------------
;; misc
;;-----------------------------------------------------------------------------

(defn mk-tag
	[id idx]
	(keyword (str "tag-" id "-" idx)))

;;-----------------------------------------------------------------------------
