(ns seven-guis.counter
  (:require [reagent.core :as r]))

(defn counter []
  (let [c (r/atom 0)]
    (fn []
      [:div
       [:label @c]
       [:button {:on-click #(swap! c inc )} "increment"]])))
