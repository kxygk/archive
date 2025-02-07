(ns
    gauge
  "Managing and plotting rain gauge data"
  (:use ;;steroids
        geoprim
        ;;geogrid
        geogrid2svg
        geogrid4image
        geogrid4seq
        svgmaps
        ible
        clojure.data.csv)
  (:require csv
            rainusage
            [town.lilac.flex
             :refer [source
                     signal]
             :rename {source $$$
                      signal $$>}]
            [town.lilac.flex.memo :as flex.memo]
            quickthing
            [missionary.core      :as m]
            ;;[injest.path :refer [+> +>> x>> =>>]]
            [thi.ng.geom.viz.core :as viz]
            [thi.ng.geom.svg.core :as svg]
            [tick.core      :as tick]))

(def params
  "The state atom"
  ($$$ {:input-dir   "./data/gauge/"
        :id-2-name   {1645240 5
                      8373316 3
                      7598252 4
                      1683136 2
                      8371400 1
                      1641660 6
                      7598984 7}
        :inside-d18o  "irregular-cave-samples.csv"
        :inside-machine-d18o "test.csv"
        :outside-d18o "manual-rain-samples.csv"
        :plot-height 400
        :plot-width 1000
        :blah        "blah"}))

#_
(sub
  params
  :input-dir)
;; => #<SyncSignal@10d6d064: "./data/gauge/">


(def
  file-paths
  ($$> (->> @($$&
               params
               :input-dir)
            java.io.File.
            .list
            sort
            (mapv #(str
                     @($$& params
                           :input-dir)
                     %)))))
#_
(identity
  @file-paths)
;; => ("./data/gauge/00rainLog.txt"
;;     "./data/gauge/00rainLog2.txt"
;;     "./data/gauge/00rainLog3.txt"
;;     "./data/gauge/00rainLog4.txt"
;;     "./data/gauge/00rainLog5.txt"
;;     "./data/gauge/00rainLog6.txt"
;;     "./data/gauge/01rainLog-b1.txt"
;;     "./data/gauge/01rainLog-b2.txt"
;;     "./data/gauge/01rainLog-b3.txt"
;;     "./data/gauge/01rainLog-b4.txt"
;;     "./data/gauge/01rainLog-b5.txt"
;;     "./data/gauge/01rainLog-b6.txt"
;;     "./data/gauge/02rainLog-a1.txt"
;;     "./data/gauge/02rainLog-a2.txt"
;;     "./data/gauge/02rainLog-a3.txt"
;;     "./data/gauge/02rainLog-a4.txt"
;;     "./data/gauge/02rainLog-a5.txt"
;;     "./data/gauge/02rainLog-a6.txt"
;;     "./data/gauge/03rainLog-1b.txt"
;;     "./data/gauge/03rainLog-2b.txt"
;;     "./data/gauge/03rainLog-3b.txt"
;;     "./data/gauge/03rainLog-4b.txt"
;;     "./data/gauge/03rainLog-5b.txt"
;;     "./data/gauge/03rainLog-6b.txt"
;;     "./data/gauge/04rainLog-1a.txt"
;;     "./data/gauge/04rainLog-2a.txt"
;;     "./data/gauge/04rainLog-3a.txt"
;;     "./data/gauge/04rainLog-4a.txt"
;;     "./data/gauge/04rainLog-5a.txt"
;;     "./data/gauge/04rainLog-6a.txt")


(def rain-logs
  ($$> (->> @file-paths
            (mapcat rainusage/read-file)
      (mapv (fn [log]
              (update log
                      :id
                      #(get @($$& params
                                  :id-2-name)
                            %))))
      (group-by :id)
      (into {}))))

(defn start-of-day
  "Given a 'time-str'
  Return the `tick/inst` of the start of that day"
  [time-str]
  (-> time-str
      (str "T00:00")
      tick/date-time
      tick/instant))

(defn get-last-log-time
  "Go through all the logs
  and find the time (as `tick/inst`) of the last log"
  [event-logs]
  (-> (apply max
             (->> event-logs
                  (mapcat (fn [one-log]
                            (->> one-log
                                 second
                                 (mapv (fn [log-entry]
                                         (-> log-entry
                                             :unix))))))))
    (tick/new-duration :seconds)
    tick/instant))

(defn event-times
  "Extract the event times from the log
  Put into a vector of 1-D vectors
  [[`tick/inst`]
   [`tick/inst`]
   [`tick/inst`]
   [`tick/inst`]]"
  [log]
  (->> log
       (mapv :unix)
    set
    (into [])
    sort
    (mapv #(-> %
               (tick/new-duration :seconds)
               tick/instant))
    (mapv vector)))


(defn events-2-rates
  "Take a series of event,
  and based on their time differences
  determine an average rate of water
  going into the gauge
  Can be optionally given a `threshold`
  and values are clipped to it"
  ([volume
    events]
   (events-2-rates volume
                   Double/MAX_VALUE
                   events))
  ([volume
    threshold
    events]
   (->> events
        (partition 2
                   1)
        (mapcat (fn [[[start-time]
                      [end-time]]]
                  (let [time-between-events (-> (tick/between start-time
                                                              end-time)
                                                tick/seconds)
                        rate                (if (zero? time-between-events)
                                              (do (println "Zero event")
                                                  0.0)
                                              (/ volume 
                                                 time-between-events))
                        thresholded         (if (> rate
                                                   threshold)
                                              threshold
                                              rate)]
                    [[start-time
                      thresholded]
                     [end-time
                      thresholded]])))
        (into []))))

(defn events-2-daily-total
  "Take a series of event,
  and count how many happen per day"
  ([start-time
    events]
   (->> events
        (mapv (fn [[event-time
                    & remaining-data-will-be-thrown-away]]
                (tick/between start-time
                              event-time)))
     ;; (mapv
     ;;   tick/days))))
     (group-by tick/days)
     (into [])
     sort
     (mapv #(update %
                    1
                    count)))))
#_
(events-2-daily-total
  (tick/+
    (tick/now)
    (tick/new-duration
      2
      :seconds))
  [[(tick/+
     (tick/now)
     (tick/new-duration
       920982
       :seconds))]
   [(tick/+
     (tick/now)
     (tick/new-duration
       920989
       :seconds))]
   [(tick/+
     (tick/now)
     (tick/new-duration
       9923112
       :seconds))]
   [(tick/+
     (tick/now)
     (tick/new-duration
       163492083
       :seconds))]])


(defn seconds-2-days
  "Take a vector of N-D vectors
  [[time1 data1 ...]
   [time2 data2 ...]
   [time3 data3 ...]
   ..]
  Convert the time values
  (ie. first vector values)
  from `seconds` to `days`"
  [rates]
  (->> rates
       (mapv #(update
                %
                0
                (partial *
                         (/ 1.0
                            (* 24
                               60
                               60)))))))
#_
(seconds-2-days
  [[(->>
      "2023-01-01"
      start-of-day
      (tick/between
        (tick/epoch))
      tick/seconds)
    9999]])
#_
(seconds-2-days
  [[13050
    9999]])


(defn cut-timeseries
  "Truncates a vector of data points to a time window.
  Points can be 1 or more dimensions
  The first dimension is the time `tick/inst`
  [[time1 x1 y1 z1 ...]
   [time2 x2 y2 z2 ...]
   ...
   [timeN xN yN zN ...]]" 
  [start-time
   end-time
   data-vec]
  (let [dimensions (count (first
                            data-vec))]
    (->> data-vec
         (filterv (fn [[time & the-rest]]
                    (and (tick/> time
                                 start-time)
                         (tick/< time
                                 end-time)))))))
#_
(cut-timeseries (tick/+ (tick/now)
                        (tick/new-duration 10
                                           :seconds))
                (tick/+ (tick/now)
                        (tick/new-duration 15
                                           :seconds))
                [[(tick/+ (tick/now)
                          (tick/new-duration 9
                                             :seconds))
                  4]
                 [(tick/+ (tick/now)
                          (tick/new-duration 12
                                             :seconds))
                  3]
                 [(tick/+ (tick/now)
                          (tick/new-duration 14
                                             :seconds))
                  5]
                 [(tick/+ (tick/now)
                          (tick/new-duration 16
                                             :seconds))
                  2]])
;; => [[#inst "2023-03-19T06:23:23.753302000-00:00" 3]
;;     [#inst "2023-03-19T06:23:25.753330000-00:00" 5]]

(defn pad-timeseries
  "Pads a time series with a `start-time` and `end-time`
  Time dimensions are in `tick/inst`
  [[start-time 0 0 0 ...]
   [time1 x1 y1 z1 ...]
   [time2 x2 y2 z2 ...]
   ...
   [timeN xN yN zN ...]
   [end-time 0 0 0 ...]]
  Also inserts zero-rate points at start and end" 
  [start-time
   end-time
   data-vec]
  (let [dimensions (count (first data-vec))]
    (->> data-vec
         (#(cons (into [start-time]
                       (repeat (dec dimensions)
                               0))
          %));; builds vec [start-time 0 0 0 ..]
      (into [])
      (#(conj %
              (into [end-time]
                    (repeat (dec dimensions)
                            0)))))))
#_
(pad-timeseries (tick/+ (tick/now)
                        (tick/new-duration 10
                                           :seconds))
                (tick/+ (tick/now)
                        (tick/new-duration 15
                                           :seconds))
                [[(tick/+ (tick/now)
                          (tick/new-duration 12
                                             :seconds))
                  3]
                 [(tick/+ (tick/now)
                          (tick/new-duration 14
                                             :seconds))
                  5]])
;; => [[#inst "2023-03-19T06:24:21.794992000-00:00" 0]
;;     [#inst "2023-03-19T06:24:23.795453000-00:00" 3]
;;     [#inst "2023-03-19T06:24:25.795811000-00:00" 5]
;;     [#inst "2023-03-19T06:24:26.795420000-00:00" 0]]

(defn timeseries-2-secs
  "Timeseries should be a vector of N-dimensional data points
  Where the first element is a `tick/inst`
  This function, given a `start-time`, converts each data point's
  first element to a number of seconds since `start-time`"
  ([data-vec]
   (timeseries-2-secs (-> data-vec
                          first
                          first) ;; `tick/inst` of the first data point
                      data-vec))
  ([start-time
    data-vec]
   (->> data-vec
        (mapv (fn [data-point]
                (update data-point
                        0
                        #(->> %
                              (tick/between
                                start-time)
                              tick/seconds)))))))
#_
(timeseries-2-days (tick/+ (tick/now)
                           (tick/new-duration 6
                                              :seconds))
  [[(tick/+ (tick/now)
            (tick/new-duration 9
                               :seconds))
    4]
   [(tick/+ (tick/now)
            (tick/new-duration 12
                               :seconds))
    3]
   [(tick/+ (tick/now)
            (tick/new-duration 14
                               :seconds))
    5]
   [(tick/+ (tick/now)
            (tick/new-duration 16
                               :seconds))
    2]])
;; => [[3 4] [6 3] [8 5] [10 2]]

(defn unix-time
  "The unix time of the start of a certain date string
  ex: 2023-01-01"
  [date]
  (-> date
      (str "T00:00")
      tick/instant
      (tick/between (tick/epoch))
      tick/seconds
      abs))
#_
(unix-time
  "2023-01-01")
;; => 1672502400

(def outside-d18o-logs
  ($$> (->>
         @($$& params
               :outside-d18o)
         csv/file-2-map
         (group-by :Location))))
#_
(keys @outside-d18o-logs)
;; => ("X" "S" "D" "B" "C")

(def inside-d18o-logs
  ($$> (->> @($$& params
                  :inside-d18o)
      csv/file-2-map
      (group-by :Location))))
#_
(keys @inside-d18o-logs)
;; => ("D" "A" "B" "C")

(def inside-time-d18o
  ($$> (->> @inside-d18o-logs
            (mapcat (fn [[location
                          log]]
                      {location (->> log
                                     (mapv (fn [log-entry]
                                             [(start-of-day
                                                (:CollectionTime
                                                 log-entry))
                                              (-> log-entry
                                                  :d18O
                                                  (Double/parseDouble)
                                                  )])))}))
            (into {}))))
#_
(identity
  @inside-time-d18o)
;; => {"D" [[#inst "2022-11-27T16:00:00.000000000-00:00" -5.75]],
;;     "A" [[#inst "2022-11-27T16:00:00.000000000-00:00" -6.0]],
;;     "B"
;;     [[#inst "2022-11-29T16:00:00.000000000-00:00" -6.74]
;;      [#inst "2022-12-05T16:00:00.000000000-00:00" -6.73]
;;      [#inst "2022-12-12T16:00:00.000000000-00:00" -6.4]
;;      [#inst "2022-12-18T16:00:00.000000000-00:00" -5.83]],
;;     "C"
;;     [[#inst "2022-11-29T16:00:00.000000000-00:00" -6.8]
;;      [#inst "2022-12-05T16:00:00.000000000-00:00" -6.46]
;;      [#inst "2022-12-12T16:00:00.000000000-00:00" -6.17]
;;      [#inst "2022-12-18T16:00:00.000000000-00:00" -5.84]]}

(def inside-machine-d18o-logs
  ($$> (->> @($$& params
                  :inside-machine-d18o)
            csv/file-2-map)))
#_
(identity
  @inside-machine-d18o-logs)

(def inside-machine-time-d18o
  ($$> (->> @inside-machine-d18o-logs
            (mapv (fn [log-entry]
                    [(->
                       (:StartTime
                        log-entry)
                       tick/date-time
                       tick/instant)
                     (-> log-entry
                         :d18O
                         (Double/parseDouble))])))))
#_
(identity
  @inside-machine-time-d18o)


(def
  outside-d18o-logs
  ($$> (->> @($$& params
                  :outside-d18o)
      csv/file-2-map
      (group-by :Location))))
#_
(keys @outside-d18o-logs)
;; => ("X" "S" "D" "B" "C")
#_
(->> (get @outside-d18o-logs
          "X")
     (mapv :CollectionTime))
;; => ["2022-12-01"
;;     "2022-12-02"
;;     "2022-12-03"
;;     "2022-12-04"
;;     "2022-12-05"
;;     "2022-12-07"
;;     "2022-12-08"
;;     "2022-12-09"
;;     "2022-12-10"
;;     "2022-12-12"
;;     "2022-12-13"
;;     "2022-12-18"]

(def outside-time-d18o
  ($$> (->> @outside-d18o-logs
            (mapcat (fn [[location
                          log]]
                      {location (->> log
                                     (mapv (fn [log-entry]
                                             [(start-of-day
                                                (:CollectionTime
                                                 log-entry))
                                              (-> log-entry
                                                  :d18O
                                                  (Double/parseDouble))]
                                              )))}))
            (into {}))))
#_
(identity @outside-time-d18o)
;; => {"X"
;;     [[#inst "2022-11-30T16:00:00.000000000-00:00" -4.79]
;;      [#inst "2022-12-01T16:00:00.000000000-00:00" -4.75]
;;      [#inst "2022-12-02T16:00:00.000000000-00:00" -2.68]
;;      [#inst "2022-12-03T16:00:00.000000000-00:00" -7.69]
;;      [#inst "2022-12-04T16:00:00.000000000-00:00" -9.41]
;;      [#inst "2022-12-06T16:00:00.000000000-00:00" -6.48]
;;      [#inst "2022-12-07T16:00:00.000000000-00:00" -6.94]
;;      [#inst "2022-12-08T16:00:00.000000000-00:00" -9.96]
;;      [#inst "2022-12-09T16:00:00.000000000-00:00" -4.67]
;;      [#inst "2022-12-11T16:00:00.000000000-00:00" -2.22]
;;      [#inst "2022-12-12T16:00:00.000000000-00:00" -4.1]
;;      [#inst "2022-12-17T16:00:00.000000000-00:00" -3.03]],
;;     "S"
;;     [[#inst "2022-11-30T16:00:00.000000000-00:00" -4.81]
;;      [#inst "2022-12-01T16:00:00.000000000-00:00" -4.74]
;;      [#inst "2022-12-02T16:00:00.000000000-00:00" -2.65]
;;      [#inst "2022-12-03T16:00:00.000000000-00:00" -7.88]
;;      [#inst "2022-12-04T16:00:00.000000000-00:00" -10.6]
;;      [#inst "2022-12-06T16:00:00.000000000-00:00" -5.64]
;;      [#inst "2022-12-07T16:00:00.000000000-00:00" -7.66]
;;      [#inst "2022-12-08T16:00:00.000000000-00:00" -9.49]
;;      [#inst "2022-12-09T16:00:00.000000000-00:00" -5.14]
;;      [#inst "2022-12-11T16:00:00.000000000-00:00" -2.24]
;;      [#inst "2022-12-12T16:00:00.000000000-00:00" -3.89]],
;;     "D"
;;     [[#inst "2022-11-30T16:00:00.000000000-00:00" -4.66]
;;      [#inst "2022-12-01T16:00:00.000000000-00:00" -4.59]
;;      [#inst "2022-12-02T16:00:00.000000000-00:00" -2.58]
;;      [#inst "2022-12-03T16:00:00.000000000-00:00" -6.68]
;;      [#inst "2022-12-04T16:00:00.000000000-00:00" -9.69]
;;      [#inst "2022-12-06T16:00:00.000000000-00:00" -6.29]
;;      [#inst "2022-12-07T16:00:00.000000000-00:00" -7.67]
;;      [#inst "2022-12-08T16:00:00.000000000-00:00" -9.73]
;;      [#inst "2022-12-09T16:00:00.000000000-00:00" -5.54]
;;      [#inst "2022-12-11T16:00:00.000000000-00:00" -1.77]
;;      [#inst "2022-12-12T16:00:00.000000000-00:00" -3.94]
;;      [#inst "2022-12-17T16:00:00.000000000-00:00" -4.57]
;;      [#inst "2022-12-18T16:00:00.000000000-00:00" -2.81]],
;;     "B" [[#inst "2022-12-18T16:00:00.000000000-00:00" -5.83]],
;;     "C" [[#inst "2022-12-18T16:00:00.000000000-00:00" -5.84]]}

(defn secondary-line-plot
  "Take a list of events - ie. rain gauge clicks
  turn it into a event log
  plot to a SVG
  Specify plot range with a `start-time` and `end-time`
  in seconds since epoch (Unix Time)"
  [data-vec
   & [{:keys [title
              x-axis-str
              y-axis-str ;; names for the axis
              width
              height
              y-name
              title
              color]
       :or   {margin-frac 0.15
              scale       36
              title       ""
              color       "#0008"}
       :as   options}]]
      (-> data-vec
          (quickthing/secondary-axis options)
          (update :data
                  #(conj %
                         {:values  data-vec
                          :attribs {:stroke "#b00"}
                          :layout  viz/svg-line-plot}))
          (viz/svg-plot2d-cartesian)))
;; Note: My current plotting framework
;; doesn't play nice with "filled" plots


;; ALL RAINGAUGE PLOTS (RATES)
(let [start-time (start-of-day "2023-01-01")
      end-time   (get-last-log-time @rain-logs)
      total-days (tick/days (tick/between start-time
                                          end-time))
      options    {:width       @($$& params
                                     :plot-width)
                  :height      @($$& params
                                     :plot-height)
                  :scale       36
                  :margin-frac 0.15}] 
  (->> @rain-logs
       ;;    logs-2-rates
       (mapv (fn [[location-id
                   location-log]]
               (let [rates (->>
                             location-log
                             event-times
                             (cut-timeseries start-time
                                             end-time)
                             (pad-timeseries start-time
                                             end-time)
                          ;;1.0 ;; mm of rain per click
                          ;; TODO - pull this into the atom!!!!!!!!!!
                             (events-2-rates (* 60.0 ;;sec-per-day
                                                60.0
                                                24.0))
                      (timeseries-2-secs start-time)
                      seconds-2-days)
              averages (->> location-log
                            event-times
                            (cut-timeseries start-time
                                            end-time)
                            #_;; counts as extra "events"
                            (pad-timeseries start-time
                                            end-time)
                         (events-2-daily-total start-time)
                         ;;pads average vector
                         (into [[0
                                 0]])
                         (#(conj %
                                 [total-days
                                  0])))]
          (let [axis (quickthing/zero-axis averages
                                          (merge options
                                                 {:x-name "DAYS"
                                                  :y-name "DRIP RATE"
                                                  :title  (str
                                                            "Rain Gauge: "
                                                            location-id)
                                                  :color  "#0008"}))]
            (spit (str location-id
                       ".svg")
              (-> (svg/group
                    {}
                    (-> axis
                        (update :data
                                #(conj %
                                       {:values  averages
                                        :attribs {:fill "#fbb"
                                                  :stroke "#fbb"
                                                  :stroke-width (str
                                                                  10 ;;bar-width
                                                                  "px")}
                          :layout  viz/svg-bar-plot}))
                        (viz/svg-plot2d-cartesian))
                    (-> axis
                        (update :data
                                #(conj %
                                       {:values  rates
                                        :attribs {:stroke "#b00"}
                                        :layout  viz/svg-line-plot}))
                        (viz/svg-plot2d-cartesian)))
                  (quickthing/svg-wrap [@($$& params
                                              :plot-width)
                                        @($$& params
                                              :plot-height)])
                  quickthing/svg2xml))))))))




(def in-cave
  ($$$ {:start (start-of-day "2022-11-29")
        :end  (start-of-day "2022-12-20")
        :options {:width       @($$& params
                                     :plot-width)
               :height      @($$& params
                                  :plot-height)
               :scale       36
               :margin-frac 0.15}
     :location 3}))

(def in-cave-duration
  ($$$ (tick/days (tick/between @($$& in-cave
                                     :start)
                               @($$& in-cave
                                     :end)))))

(def in-cave-drip-log
  ($$> (get
         @rain-logs
         @($$& in-cave
               :location))))

(def in-cave-rates
  ($$> @in-cave-drip-log))

(def in-cave-drip-rates
  ($$> (->> @in-cave-rates
            event-times
            (cut-timeseries @($$& in-cave
                                  :start)
                            @($$& in-cave
                                  :end))
      (pad-timeseries @($$& in-cave
                            :start)
                      @($$& in-cave
                            :end))
      ;;1.0 ;; mm of rain per click
      ;; TODO - pull this into the atom!!!!!!!!!!
      (events-2-rates (* 60.0 ;;sec-per-day
                         60.0
                         24.0))
      (timeseries-2-secs @($$& in-cave
                               :start))
      seconds-2-days)))

(def in-cave-daily-averages
  ($$> (->> @in-cave-rates
            event-times
            (cut-timeseries @($$& in-cave
                                  :start)
                            @($$& in-cave
                                  :end))
      #_(pad-timeseries ;; counts as extra "events"
          start-time
          end-time)
      (events-2-daily-total @($$& in-cave
                                  :start))
      ;;pads average vector
      (into [[0
              0]])
      (#(conj %
              [@in-cave-duration
               0])))))

(def
  in-cave-isotopes-tick
  ($$> (->> @inside-machine-time-d18o
            (cut-timeseries @($$& in-cave
                                  :start)
                            @($$& in-cave
                                  :end)))))

(def main-drip-rate-axis
  ($$> (quickthing/zero-axis @in-cave-daily-averages
                            (merge @($$& in-cave
                                         :options)
                                   {:x-name "DAYS"
                                    :y-name "DRIP RATE"
                                    :title  (str
                                              "In Cave ONSITE ")
                                    :color  "#b00"}))))

(def secondary-isotope-axis
  ($$> (quickthing/secondary-axis (->> @in-cave-isotopes-tick
                                       (pad-timeseries @($$& in-cave
                                                             :start)
                                                       @($$& in-cave
                                                          :end))
                                       (timeseries-2-secs @($$& in-cave
                                                                :start))
                                       seconds-2-days
                                       (cons [0
                                              -7.0]))
      (merge @($$& in-cave
                   :options)
        {;;:x-name "DAYS"
         :y-name "d18O"
         :title  (str "In Cave ONSITE ")
         :color  "blue"}))))

(def in-cave-isotope
  ($$> (->> @in-cave-isotopes-tick
            #_(pad-timeseries
                start-time
                end-time)
            (timeseries-2-secs @($$& in-cave
                                     :start))
            seconds-2-days)))

  ;;   ;; IN CAVE - FIRST ONSITE
  ;; (let [start-time   (start-of-day
  ;;                      "2022-11-28")
  ;;       end-time     (start-of-day
  ;;                      "2022-12-20")
  ;;       total-days   (tick/days (tick/between
  ;;                                 start-time
  ;;                                 end-time))
  ;;       options      {:width       @(sub
  ;;                                     params
  ;;                                     :plot-width)
  ;;                     :height      @(sub
  ;;                                     params
  ;;                                     :plot-height)
  ;;                     :scale       36
  ;;                     :margin-frac 0.15}
  ;;       location-log (get
  ;;                      @rain-logs
  ;;                      3)] 
  ;; (let [rates    (->>
  ;;                  location-log
  ;;                  event-times
  ;;                  (cut-timeseries
  ;;                    start-time
  ;;                    end-time)
  ;;                  (pad-timeseries
  ;;                    start-time
  ;;                    end-time)
  ;;                  (events-2-rates
  ;;                    (*
  ;;                      ;;1.0 ;; mm of rain per click
  ;;                      ;; TODO - pull this into the atom!!!!!!!!!!
  ;;                      60.0 ;;sec-per-day
  ;;                      60.0
  ;;                      24.0))
  ;;                  (timeseries-2-secs
  ;;                    start-time)
  ;;                  seconds-2-days)
  ;;       averages (->>
  ;;                  location-log
  ;;                  event-times
  ;;                  (cut-timeseries
  ;;                    start-time
  ;;                    end-time)
  ;;                  #_(pad-timeseries ;; counts as extra "events"
  ;;                      start-time
  ;;                      end-time)
  ;;                  (events-2-daily-total
  ;;                    start-time)
  ;;                  (into ;;pads average vector
  ;;                    [[0 0]])
  ;;                  (#(conj
  ;;                      %
  ;;                      [total-days
  ;;                       0])))
  ;;       isotopes-tick (->>
  ;;                  @inside-machine-time-d18o
  ;;                  (cut-timeseries
  ;;                    start-time
  ;;                    end-time)#_#_#_
  ;;                  (pad-timeseries
  ;;                    start-time
  ;;                    end-time)
  ;;                  (timeseries-2-secs
  ;;                    start-time)
  ;;                  seconds-2-days)
  ;;                  ]
  ;;   (let [axis         (quickthing/new-axis
  ;;                        averages
  ;;                        (merge
  ;;                          options
  ;;                          {:x-name "DAYS"
  ;;                           :y-name "DRIP RATE"
  ;;                           :title  (str
  ;;                                     "In Cave ONSITE ")
  ;;                           :color  "#b00"}))
  ;;         isotope-axis (quickthing/secondary-axis
  ;;                        (->>
  ;;                          isotopes-tick
  ;;                          (pad-timeseries
  ;;                            start-time
  ;;                            end-time)
  ;;                          (timeseries-2-secs
  ;;                            start-time)
  ;;                          seconds-2-days
  ;;                          (cons
  ;;                            [0 -7.0])
  ;;                            )
  ;;                        (merge
  ;;                          options
  ;;                          {;;:x-name "DAYS"
  ;;                           :y-name "d18O"
  ;;                           :title  (str
  ;;                                     "In Cave ONSITE ")
  ;;                           :color  "blue"}))]
(spit (str 3
           "onsite.svg")
      (-> (svg/group
            {}
            (-> @main-drip-rate-axis
                (update :data
                        #(conj %
                               {:values  @in-cave-daily-averages
                                :attribs {:fill         "#fbb"
                                          :stroke       "#fbb"
                                          :stroke-width (str
                                                          10 ;;bar-width
                                                          "px")}
                                :layout  viz/svg-bar-plot}))
                (viz/svg-plot2d-cartesian))
            (-> @main-drip-rate-axis
                (update :data
                        #(conj %
                               {:values  @in-cave-drip-rates
                                :attribs {:stroke "#b00"}
                                :layout  viz/svg-line-plot}))
                (viz/svg-plot2d-cartesian))
            (-> @secondary-isotope-axis
                (update :data
                        #(conj %
                               {:values  @in-cave-isotope
                                :attribs {:stroke "blue"}
                                :layout  viz/svg-line-plot}))
                (viz/svg-plot2d-cartesian)))
          (quickthing/svg-wrap [@($$& params
                                      :plot-width)
                                @($$& params
                                      :plot-height)])
          quickthing/svg2xml))

;; IN CAVE
#_ ;; TODO: Fix the `standard-bar-plot`.. not sure what that was before...
(let [start-time (start-of-day
"2022-11-08")
end-time         (start-of-day
"2022-12-30")]
  (->> 
    @rain-logs
;;    logs-2-rates
    (mapv ;;run!
      (fn [[location-id
            location-log]]
          (spit
            (str
              location-id
              ".svg")
            (->
              (svg/group
                {}
                ;; RATE PLOT
                (->
                  (->>
                    location-log
                    event-times
                    (cut-timeseries-and-pad 
                      start-time
                      end-time)
                    (events-2-daily-total
                      start-time))
                (standard-bar-plot               
                  {:x-name "DAYS"
                   :y-name "DRIP RATE"
                   :bar-width 12
                   :width  (:plot-width
                            @params)
                   :height (:plot-height
                            @params)
                   :title  (str
                             "Rain Gauge: "
                             location-id
                            )}))
              ;; ISOTOPE PLOT
                (->
                  (->>
                    location-log
                    event-times
                    (cut-timeseries-and-pad 
                      start-time
                      end-time)
                    (events-2-rates
                      (*
                        0.2 ;; mm of rain per click
                        ;; TODO - pull this into the atom!!!!!!!!!!!!!!!!
                        60.0 ;;sec-per-day
                        60.0
                        24.0))
                    timeseries-2-secs
                    seconds-2-days)
                (secondary-plot               
                  {:x-name "BOBA"
                   :y-name "PEEPEE"
                   :width  (:plot-width
                            @params)
                   :height (:plot-height
                            @params)
                   :title  (str
                             "Rain Gauge: "
                             location-id)})))
             (quickthing/svg-wrap
                [(:plot-width
                  @params)
                 (:plot-height
                  @params)])
            quickthing/svg2xml))))))

















#_
(let [start-time (start-of-day
                   "2023-01-01")
      end-time   (get-last-log-time
                   (rain-logs))]
  (let [drip-rate-plot  (->> 
                          (rain-logs)
                          logs-2-rates
                          (run!
                            (fn [[id
                                  rates]]
                              (spit
                                (str
                                  id
                                  ".svg")
                                (->
                                  (plot-rate
                                    (->>
                                      rates
                                      (cut-rate-timestamps
                                        start-time
                                        end-time)
                                      normalize-time)
                                    (str
                                      "Rain Gauge: "
                                      id)))))))
        d18o-plot (->> 5)]
    (->
      (group {}
             drip-rate-plot
             d18o-plot)
      (quickthing/svg-wrap
        [(:plot-width
          @params)
         (:plot-height
          @params)])
      quickthing/svg2xml)))



#_
(defn
  plot-daily-average
  "Take a list of events - ie. rain gauge clicks
  turn it into a event log
  plot to a SVG
  Specify plot range with a `start-time` and `end-time`
  in seconds since epoch (Unix Time)"
  [event-log
   start-time
   end-time
   title]
  (let [normalized-log (->>
                         event-log
                         (cut-to-time-window
                           start-time
                           end-time)
                         normalize-log)]
    (svg/group
      {}
      (->
         normalized-log
         (quickthing/standard-axis
           (:plot-width
            @params)
           (:plot-height
            @params)
           {:x-name "DAYS"
            :y-name "DRIP RATE"
            :title  title})
         (update
           :data
           #(conj
              %
              {:values  normalized-log
               :attribs {:stroke "#b00"}
               :layout  viz/svg-line-plot}))
         (viz/svg-plot2d-cartesian))
      (->
         normalized-log
         (quickthing/secondary-axis
           (:plot-width
            @params)
           (:plot-height
            @params)
           {;;:x-name "BUTTS"
            :y-name "BOBAS"
            :title  title
            :color "#0004"})
         (update
           :data
           #(conj
              %
              {:values  normalized-log
               :attribs {:stroke "#b00"}
               :layout  viz/svg-line-plot}))
         (viz/svg-plot2d-cartesian)))))
;; Note: My current plotting framework
;; doesn't play nice with "filled" plots




#_
(let [start-time (unix-time
                   "2023-01-01")
      end-time   (get-last-log-time
                   (rain-logs))] 
  (->> 
    (rain-logs)
    (run!
      (fn [[id
            event-log]]
        (spit
          (str
            id
            ".svg")
          (->
            event-log
            (plot-rate
              start-time
              end-time
              (str
                "Rain Gauge: "
                id))
            (quickthing/svg-wrap
              [(:plot-width
                @params)
               (:plot-height
                @params)])
            quickthing/svg2xml))))))

#_
(let [start-time (unix-time
                   "2022-12-08")
      end-time   (unix-time
                   "2022-12-30")]
  (spit
    (str
      "manual-collection-outside.svg")
    (->
      (get
        (rain-logs)
        5)
      (plot-rate
        start-time
        end-time
        (str
          "Manually Collected Outside"))
      (quickthing/svg-wrap
        [(:plot-width
          @params)
         (:plot-height
          @params)])
      quickthing/svg2xml)))
