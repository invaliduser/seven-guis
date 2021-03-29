(ns ^:figwheel-hooks seven-guis.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as r]
   [reagent.dom :as rd]
   [seven-guis.counter :as counter]
   [seven-guis.temperature :as temperature]
   [seven-guis.flight :as flight]
   [seven-guis.timer :as timer]
   [seven-guis.crud :as crud]
   [seven-guis.circle :as circle]
   [seven-guis.cells :as cells]))

(println "This text is printed from src/seven_guis/core.cljs. Go ahead and edit it and see reloading in action.")

(defn multiply [a b] (* a b))

;; define your app data so that it doesn't get over-written on reload
(defonce app-state (atom {:text "Hello world!"}))

(defn get-app-element []
  (gdom/getElement "app"))



(defn guis []
  [:div
   [:div [:h3 "Counter"]
    [:div [counter/counter]]]
   [:hr]

   [:div [:h3 "Temperature"]
    [:div [temperature/temperature]]]
   [:hr]

   [:div [:h3 "Flight"]
    [:div [flight/flight]]]
   [:hr]
   
   [:div [:h3 "Timer"]
    [:div [timer/timer]]]
   [:hr]
   [:div [:h3 "Crud"]
    [:div [crud/crud]]]
   [:hr]
   [:div [:h3 "Circle"]
    [:div [circle/circle]]]
   [:hr]
   [:div [:h3 "Cells"]
    [:div [cells/cells]]]
   [:hr]])


;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  (rd/render [guis]
             (get-app-element))

  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

(on-reload)
