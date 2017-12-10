(ns elektronik.utils)

(defn js-merge
  "Performs a merge of js objects. Returns a new instance of js/Object."
  ([]
   #js{})
  ([x]
   (.assign js/Object #js{} x))
  ([x y]
   (.assign js/Object #js{} x y))
  ([x y & more]
   (apply (.-assign js/Object) #js{} x y more)))
