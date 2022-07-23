(ns
    geojson2svg
  "Code for reading GeoJSON files and converting to SVG for display"
  (:use geoprim)
  (:require [geo
             io
             jts]
            [thi.ng.geom.svg.core :as svg]))

(defn
  crop-to-region
  "Take a GeoJSON and crop the data to a rectangular REGION
  - Construct a cropping polygon which bounds our region of interest
  - Use the `intersect` in `geo/jts` to crop out the region to a GeoJSON
  - Flatten the GeoJSON/region into a bare list of polygons."
  [geojson
   region]
  (let [region-corners-lonlat (mapv
                                #(let [[lat
                                        lon] (as-latlon
                                               %)]
                                   [lon
                                    lat])
                                (->
                                  region
                                  four-corners))
        region-polygon        (into
                                region-corners-lonlat
                                (first
                                  region-corners-lonlat))
        area-of-interest      (geo.jts/polygon-wkt
                                [(flatten
                                   region-polygon)])]
    (->>
      geojson
      (map
        (fn
          [geojson-poly]
          (update
            geojson-poly
            :geometry
            #(geo.jts/intersection
               %
               area-of-interest))))
      (filter
        #(not
           (.isEmpty
             (:geometry
              %)))))))
;; TODO: 
;; Sometimes intersect will cut a polygon into multiple polygons, and these will be grouped into a `MultiPolygon` object. You can also get `GeometryCollection` objects (unclear when). These too should be unfolded so that you are left with a long list of `Polygons`
;;
;; There are still some corner cases where you get a `Point` instead of a `Polygon`. Or other stranger geometries that I am not handeling at the moment.

(defn
  convert
  "Convert a GeoJSON object to a SVG hiccup vector
  Extracts all the Polygons' coordinates
  and use them directly to create `thing/geom` SVG hiccup
  ..
  The SVG hiccup can then be serialized and output as an SVG string"
  [geojson
   region
   attribs]
  (->>
    geojson
    (map
      :geometry)
    (reduce
      (fn
        [polygon-seq ;; extract polygons from multipolygons
         next-geometry]
        (case
            (.getGeometryType
              next-geometry)
          ;;
          "Polygon"
          (conj
            polygon-seq
            next-geometry)
          ;;
          "MultiPolygon"
          (apply
            conj
            polygon-seq
            (geo.jts/polygons
              next-geometry))
          ;;
          "GeometryCollection" ;; should be recursive
          (apply
            conj
            polygon-seq
            (geo.jts/geometries
              next-geometry))))
      [])
    (map
      geo.jts/coordinates)
    (map
      geo.jts/coord-array) 
    (map
      (fn [poly]
        (map
          #(point-to-eassou
             region
             (point
               (.getY %)
               (.getX %)))
          poly)))
    (map
      #(svg/line-strip
         %
         attribs))
    (apply
      svg/group
      {})))
;; TODO: Make this properly recursive

(defn
  read-file
  "Read in a GeoJSON file
  crop it to a region
  return an SVG hiccup.
  DEFAULT: polygons drawn with a black contour
  Optionally provide `attribs` to override"
  ;;
  ([geojson-file
    region]
   (let [attribs
         {:stroke-width "0.05px"
          :stroke       "black"}]
   (read-file
     geojson-file
     region
     attribs)))
  ;;
  ([geojson-file
    region
    attribs]
   (->
     geojson-file
     slurp
     geo.io/read-geojson
     (crop-to-region
       region)
     (convert
       region
       attribs))))


;;= DEMO

#_
(defn-
  plot-polygon
  [plot
   [width
    height]]
  (let [aspect-ratio
        (/
          width
          height)]
    (->>
      plot
      (svg/svg
        {:width   1200.0
         :height  (/
                    1200.0
                    aspect-ratio)
         :viewBox (str
                    "0 0 "
                    width
                    " "
                    height)}))))

;; Region Around Taiwan and Chinese coast
#_
(def
  minnan-region
  (region
    (point
      26.23
      116.47)
    (point
      21.7
      125)))

#_
(def
  shoreline-filestr
  "/home/kxygk/Data/shoreline/GSHHG/shoreline.json")

#_
(spit
  "output.svg"
  (->
    shoreline-filestr
    (read-file
      minnan-region)
    (plot-polygon
      (dimension
        minnan-region))
    svg/serialize
    (clojure.string/replace
      #"><"
      ">\n<")))
