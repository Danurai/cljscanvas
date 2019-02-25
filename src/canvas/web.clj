(ns canvas.web
  (:require 
    [compojure.core     :refer [defroutes GET POST ANY context]]
    [compojure.route    :refer [resources]]
   ; [ring.util.response :refer [response resource-response content-type redirect]]
    (ring.middleware
      [params :refer [wrap-params]]
      [keyword-params :refer [wrap-keyword-params]]
      [session :refer [wrap-session]])
    [canvas.pages :as pages]))

(defroutes app-routes
  (GET "/" [] pages/home)
  (resources "/"))
   
(def app 
  (-> app-routes
    (wrap-keyword-params)
    (wrap-params)
    (wrap-session)))
   