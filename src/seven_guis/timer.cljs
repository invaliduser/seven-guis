(ns seven-guis.timer
  (:require [reagent.core :as r]))

(def elapsed-time (r/atom 0))

(def duration (r/atom 5))


(defn advance-time! []
  (cond (> @duration @elapsed-time)
        (let [v (min (js/Number @duration) (+ (js/Number @elapsed-time) .1))]
          (reset! elapsed-time v))))

(defonce timer-id (atom nil))

(defn start-timer! []
  (if-let [old-interval @timer-id]
    (do #_(println "extant timer-id: "@timer-id)
        (js/clearInterval old-interval)))
  (let [new-id (js/setInterval advance-time! 100)]
    (reset! timer-id new-id)))


(defn timer []
  [:div.container 

   [:div [:label "Elapsed Time:"] [:meter {:min 0
                                           :max @duration
                                           :value @elapsed-time}

                                   ]] ;gauge el
   [:div [:label (as-> @elapsed-time chronos
                     (* chronos 10)
                     (.round js/Math chronos)
                     (/ chronos 10)
                     (str chronos)) " s"]]

   [:div [:label "Duration:"]  [:input {:type "range"
                                        :value @duration
                                        :max 20
                                        :min 2
                                        :step 0.1
                                        :on-change #(do (reset! duration (.. % -target -value))

                                                        (let [v (min (js/Number @elapsed-time) (js/Number @duration))]
                                                          #_(if (js/isNaN v)
                                                            (println "on-change:" "duration:" @duration "elapsed-time:" @elapsed-time "result: " v))
                                                          (reset! elapsed-time v)
                                                          ))}]]

   [:div [:label "duration-numeric:" (str @duration)]]
   [:div [:button {:on-click #(do
                                (println "reset:" (type 0))
                                (reset! elapsed-time 0))}
          "Reset"]]])

(start-timer!)
