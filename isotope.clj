(ns isotope
  "plot some isotope data.."
  (:use ible       
        clojure.data.csv
        clojure.math)
  (:require csv
            quickthing
            [tech.v3.dataset :as ds]
            [tech.v3.dataset.column-filters :as cf]
            rainusage
            [missionary.core      :as m]
            ;;[injest.path :refer [+> +>> x>> =>>]]
            [thi.ng.geom.viz.core :as viz]
            [thi.ng.geom.svg.core :as svg]
            [tick.core      :as tick]
            tock))


(def manual
  (-> "data/thailand/remote/manual.csv"
      ds/->dataset
      (ds/filter-column "Location"
                        #(-> %
                             empty?
                             not))
      (ds/drop-missing "d18O")))


(def auto-inside
  (-> "data/thailand/remote/auto-inside.csv"
      ds/->dataset
      #_
      (ds/filter-column "Location"
                        #(-> %
                             empty?
                             not))
      (ds/drop-missing "d18O")))



(defn
  add-offset-days
  [dataset
   start-time]
  (let [start-time (-> start-time
                         tock/start-of-day)]
    (-> dataset
        (ds/column-map "DaysOffset"
                       (fn [vial-code
                            location
                            date-str
                            d18O
                            _
                            _
                            _
                            _]
                         (->> date-str
                              tock/start-of-day
                              (tick/between start-time)
                              tick/days))))))
#_
(-> manual
    (add-offset-days "2023-01-01")
    (ds/select-columns ["Location"
                        selected-col
                        "DaysOffset"]))


(defn
  auto-add-offset-days
  [dataset
   start-time]
  (let [start-time (-> start-time
                         tock/start-of-day)]
    (-> dataset
        (ds/column-map "DaysOffset"
                       (fn [vial-code
                            location
                            date-str
                            d18O
                            _
                            _
                            _
                            _]
                         (->> date-str
                              tock/start-of-day
                              (tick/between start-time)
                              tick/days))))))
#_
(-> auto-inside
    (add-offset-days "2023-01-01")
    (ds/select-columns ["DaysOffset"
                        "d18O"])
    ds/rowvecs)
 

(defn
  plot
  [selected-col]
  (let [location-map (->> (-> manual
                              (add-offset-days "2023-01-01")
                              (ds/select-columns ["DaysOffset"
                                                  selected-col
                                                  "Location"])
                              (ds/group-by-column "Location"))
                          (map (fn [[k
                                     v]]
                                 [k
                                  (dissoc v
                                          "Location")]))
                          (into {}))]
    (let [auto-pairs (-> auto-inside
                         (add-offset-days "2023-01-01")
                         (ds/select-columns ["DaysOffset"
                                             selected-col])
                         ds/rowvecs)
          B-pairs    (-> location-map
                         (get "B")
                         ds/rowvecs)
          C-pairs    (-> location-map
                         (get "C")
                         ds/rowvecs)
          D-pairs    (-> location-map
                         (get "D")
                         ds/rowvecs)
          X-pairs    (-> location-map
                         (get "X")
                         ds/rowvecs)
          axis-basis B-pairs 
          axis       (quickthing/new-axis axis-basis
                                          {:x-name "Days"
                                           :y-name "d18O"
                                           :title  selected-col
                                           :color  "#0004"})]
      (->> (-> (svg/group {}
                          (-> axis
                              (update :data
                                      #(conj %
                                             {:values  auto-pairs
                                              :attribs {:stroke "magenta"}
                                              :layout  viz/svg-line-plot}
                                             {:values  auto-pairs
                                              :attribs {:stroke "magenta"}
                                              :layout  viz/svg-scatter-plot}))
                              viz/svg-plot2d-cartesian)
                          (-> axis
                              (update :data
                                      #(conj %
                                             {:values  B-pairs
                                              :attribs {:stroke "blue"}
                                              :layout  viz/svg-line-plot}
                                             {:values  B-pairs
                                              :attribs {:stroke "blue"}
                                              :layout  viz/svg-scatter-plot}))
                              viz/svg-plot2d-cartesian)
                          (-> axis
                              (update :data
                                      #(conj %
                                             {:values  C-pairs
                                              :attribs {:stroke "cyan"}
                                              :layout  viz/svg-line-plot}
                                             {:values  C-pairs
                                              :attribs {:stroke "cyan"}
                                              :layout  viz/svg-scatter-plot}))
                              viz/svg-plot2d-cartesian)
                          (-> axis
                              (update :data
                                      #(conj %
                                             {:values  D-pairs
                                              :attribs {:stroke "grey"}
                                              :layout  viz/svg-line-plot}
                                             {:values  D-pairs
                                              :attribs {:stroke "grey"}
                                              :layout  viz/svg-scatter-plot}))
                              viz/svg-plot2d-cartesian)
                          (-> axis
                              (update :data
                                      #(conj %
                                             {:values  X-pairs
                                              :attribs {:stroke "red"}
                                              :layout  viz/svg-line-plot}
                                             {:values  X-pairs
                                              :attribs {:stroke "black"}
                                              :layout  viz/svg-scatter-plot}))
                              viz/svg-plot2d-cartesian))
               (quickthing/svg-wrap [1000
                                     600])
               quickthing/svg2xml)
           (spit (str "fig/plot/"
                      "cave-"
                      selected-col
                      ".svg"))))))

(plot "d18O")
(plot "dD")
