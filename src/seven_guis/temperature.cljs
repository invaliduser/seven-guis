(ns seven-guis.temperature
  (:require [reagent.core :as r]))


(def c (r/atom "0"))
(def f (r/atom "32"))
(defn c->f [n] (-> n
                   (* 1.8)
                   (+ 32)))

(defn f->c [n] (-> n
                   (- 32) 
                   (/ 1.8)))

(defn round [n]
  (-> n
      (* 10 )
      (Math/round)
      (/ 10)))

(defn on-blur [dir]
  (fn [e]
    (let [[target-atom conversion] (case dir
                                     :f->c [c f->c]
                                     :c->f [f c->f])
          el (.. e -target)
          v (.-value el)
          parsed (js/parseFloat v)]
      (reset! target-atom (str (round (conversion v)))))))

(defn on-change [atm]
  (fn [e]
    (let [v  (.. e -target -value)]
      (if (js/isNaN v)
        (do)
        (reset! atm (.. e -target -value))))))

(defn temperature []
  (fn []
    [:div
     [:label "celsius:"]
     [:input {:value @c
              :on-change (on-change c)
              :on-blur (on-blur :c->f)}]

     [:label "farenheit"]
     [:input {:value @f
         :on-change (on-change f)
              :on-blur (on-blur :f->c)}]]))
