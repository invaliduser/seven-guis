(ns seven-guis.crud
  (:require [reagent.core :as r]))


(def input-name (r/atom {:name "" :surname ""}))
(defn update-key [k]
  (fn [e]
    (swap! input-name assoc k (.. e -target -value))))

(def selected (r/atom nil))
(def filter-string (r/atom ""))

(def names (r/atom
            [{:name "Hans"
              :surname "Emil"}
             {:name "Max"
              :surname "Mustermann"}
             {:name "Roman"
              :surname "Tisch"}]))


(defn create-name! []
  (swap! names conj @input-name))

(defn update-entry! []
  (swap! names assoc (js/Number @selected) @input-name))

(defn delete-entry! []
  (let [selected (js/Number @selected)]
    (swap! names #(into (subvec % 0 selected)
                        (subvec % (inc selected))))))

                                        ;do w/flex
(defn crud []
  (let [filtered (r/track filter #(clojure.string/starts-with?
                                   (clojure.string/upper-case (:surname %))
                                   (clojure.string/upper-case @filter-string))
                          (map-indexed (fn [idx item] (assoc item :idx idx)) @names))
        nothing-selected? (r/track nil? @selected)] 

    [:div.container {:style {:width "25%"
                             :display "grid"
                             :grid-row-gap "10px"}}
     [:div.section {:style {:display "grid"
                            :grid-template-columns "50% 50%"}} ;top section

      [:div
       [:label {:style {:white-space "nowrap"}}"Filter prefix: "]
       [:input {:size 9
                :value @filter-string
                :on-change #(let [new-v  (.. % -target -value)]
                              (reset! filter-string new-v)
                              (if-not (clojure.string/starts-with?
                                       (-> (js/Number @selected) (@names) :surname)
                                       new-v)
                                (reset! selected nil)))}]]
]

     [:div.section  {:style {:display "grid"
                              :grid-template-columns "50% 50%"
                              :grid-column-gap "4px"}}                    ;middle section

      [:div.section                     ;left panel
       [:select#names {:size 4

                       :style {:flex-grow "1"
                               :min-width "100%"
                               :width "auto"}
                       :on-change #(reset! selected (.. % -target -value))}

        (doall (for [{:keys [name surname idx]} @filtered]
                [:option {:key (str surname "," name)
                          :value idx}
                 (str surname ", " name)]))]]

      [:div.section {:style {:display "flex"}}  ;right panel

       [:div.section 
        [:div [:label "Name:"]]
        [:div [:label "Surname: "]]]
       [:div.section  
        [:div [:input {:on-change (update-key :name)}]]
        [:div [:input {:on-change (update-key :surname)}]]]]]
     
     [:div.section {:style {:display "grid"
                            :grid-template-columns "auto auto auto auto auto auto"
                            :grid-column-gap "5px"}}
                                        ;bottom section/buttons
      [:button {:on-click create-name!} "Create" ]
      [:button {:on-click update-entry! :disabled @nothing-selected?} "Update"]
      [:button {:on-click delete-entry! :disabled @nothing-selected?} "Delete"]]]))
