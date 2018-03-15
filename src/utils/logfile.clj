(ns utils.logfile
    (:require 	(utils        [core    :refer :all])
                (clojure.java [io       :as io])
            	(clojure 	  [string   :as str])
              	(taoensso 	  [timbre   :as log])
               	(taoensso.timbre.appenders 	[core :as appenders])
))

;;-----------------------------------------------------------------------------

(defn- rotate-file
    [log path prefix num-files]
    (let [dir        (get-dir-list path (re-pattern (str prefix "-\\d+\\.log")))
          extract    (fn [x] (str/replace x #"^([^-]+-)([0-9]+)(\.log)$" "$2"))
          numbers    (map extract dir)
          biggest    (->> numbers (map #(Integer/valueOf %)) sort last)
          smallest   (->> numbers (map #(Integer/valueOf %)) sort first)
          new-big    (str path prefix "-" (if (nil? biggest) 0 (inc biggest)) ".log")]
        (.renameTo log (io/file new-big))
        (.createNewFile log)
        (when (>= (count numbers) num-files)
          	(io/delete-file (str path prefix "-" smallest ".log") true))))

(defn- max-size-appender
    "Returns a Rolling file appender. Opts:
    :path      - logfile path.
    :prefix    - first part of filename.
    :max-size  - max size in bytes.
    :num-files - max number of files."
    [& [{:keys [path prefix max-size num-files]
         :or   {path      "./"
                prefix    "rolling"
                max-size  1000000
                num-files 10}}]]
        {:enabled?   true
         :async?     false
         :min-level  nil
         :rate-limit nil
         :output-fn  :inherit
         :fn (fn [data]
            (let [{:keys [instant output_]} data
                  output-str (force output_)
                  filename   (str path prefix ".log")]
                (when-let [log (io/file filename)]
                    (try
                        (if-not (.exists log)
                            (io/make-parents log)
                        	(when (> (.length log) max-size)
                                (rotate-file log path prefix num-files)))
                        (spit filename (with-out-str (println output-str)) :append true)
                        (catch java.io.IOException _)))))})

(defn setup-log
    [prefix max-size num-files]
    (log/merge-config! {
        :level :trace
        :appenders {
            :println {:enabled? false}
            :spit
                (max-size-appender
                    {:path "./" :prefix prefix :max-size max-size :num-files num-files})}
        :timestamp-opts {
            :pattern "MM-dd HH:mm:ss"
            :locale (java.util.Locale. "sv_SE")
            :timezone (java.util.TimeZone/getTimeZone "Europe/Stockholm")}
        :output-fn (partial log/default-output-fn {:stacktrace-fonts {}})}))

