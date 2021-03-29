(ns seven-guis.flight
  (:require [reagent.core :as r]))

                                        ;When a non-disabled textfield T has an ill-formatted date then T is colored red and B is disabled.

                                        ;When C has the value "return flight" and T2's date is strictly before T1's then B is disabled.



;When clicking B a message is displayed informing the user of his selection (e.g. 'You have booked a one-way flight on 04.04.2014."). 

(defn event-value [e]
  (.. e -target -value))

(def flight-type (r/atom "one-way"))
(def initial-flight-text (r/atom ""))
(def return-flight-text (r/atom ""))

(def well-formed-date  #"[0-9]{1,2}\.[0-9]{1,2}\.[0-9]{4}")
(defn is-valid-date-string? [s]
  (re-matches well-formed-date s))

(defn initial-invalid? []
  (not (is-valid-date-string? @initial-flight-text)))
(defn return-invalid? []
  (and (= @flight-type "round-trip" )
       (not(is-valid-date-string? @initial-flight-text))))

(defn to-short-date-format [s]
   (clojure.string/replace s #"\." "/"))

(defn to-date [s]
  (-> s
      to-short-date-format
      (js/Date.)))

(defn possible-dates? []
  (>= (to-date @return-flight-text)
     (to-date @initial-flight-text)))

(defn book-disabled? []
  (or
   (initial-invalid?)
   (and (= @flight-type "round-trip")
        (or (return-invalid?)
         (not (possible-dates?))))))

(defn flight []
  [:div.container
   [:style ".invalid-date{background-color:red;}"]   
   #_[:div "init text here: "
    (str @initial-flight-text)]

   [:div
    [:select {:on-change #(reset! flight-type (event-value %))}
     [:option {:value "one-way"} "One-Way Flight"]
     [:option {:value "round-trip"} "Round Trip"]]]

   [:div [:input {:class (if (initial-invalid?) "invalid-date")
                  :value @initial-flight-text
                  :on-change #(reset! initial-flight-text (event-value %))}]]
   [:div [:input {:class (if (return-invalid?) "invalid-date")
                  :value @return-flight-text
                  :on-change #(reset! return-flight-text (event-value %))
                  :disabled (not= @flight-type "round-trip")}]]
   [:div [:button
          {:disabled (book-disabled?)
           :on-click #(js/alert (str "You have booked a " @flight-type " flight on " (to-short-date-format @initial-flight-text) "."))}
          "Book"]]])
