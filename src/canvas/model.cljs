(ns canvas.model
  (:require
    [reagent.core :as r]))
    
(def appstate (r/atom {:origincube [0 0 0]}))