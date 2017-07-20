(ns resume.core
  (:gen-class)
  (:refer-clojure :rename {map clj-map meta clj-meta time clj-time})
  (:require [clj-template.html5 :refer :all]
            [clojure.math.numeric-tower :as math]
            [resume.bio :as bio]))

;;;;;;;;;;;;;;;;;;;;
;; Get Data       ;;
;;;;;;;;;;;;;;;;;;;;

(defn get-data []
  ;;(load-string (slurp "models.edn")))
  (load-string (slurp "https://raw.githubusercontent.com/schmudde/schmud-de/master/src/schmud_de/models.clj")))

;;;;;;;;;;;;;;;;;;;;
;; Tools          ;;
;;;;;;;;;;;;;;;;;;;;

(defn desc-itemizer [matcher-object points]
  "I iterate through a java.util.regex.Matcher.find() object and turn each returned string into an HTML list item."
  (let [single-point (re-find matcher-object)]
    (if (some? single-point)
      (desc-itemizer matcher-object (str points (li {:class "qualification"} single-point)))
      points)))

(defn project-finder [search-term db keyword-search]
  (if (= ((first db) (keyword keyword-search)) search-term)
    (first db)
    (project-finder search-term (rest db) keyword-search)))

;;;;;;;;;;;;;;;;;;;;
;; Layouts        ;;
;;;;;;;;;;;;;;;;;;;;

(defn desc-layout [description]
  "I create a HTML list. I expect a string of data delimited by semicolons."
  (ul {:class "qualifications"} (desc-itemizer (re-matcher #"\w[^;]+" description) "")))

(defn column-layout [talent title description]
  "I take parameters and build a single entry"
  (div {:class "col-xs-4"}
       (h2 talent)
       (h3 title)
       (p description)))

(defn single-column-layout
  "I take parameters and build a single entry"
  [title subtitle description date & [class-synopsis bullet-points technology etc]]
  (div {:class "col-xs-12 job"}
       (h2 title (small {:class "pull-right"} date))
       (h3 description (small subtitle) (small technology))
       (p class-synopsis)
       (if (nil? bullet-points) nil (desc-layout bullet-points))
       etc))

(defn speaking-layout [talk]
  (li
   (p (strong (talk :title)))
   (p (talk :location))))

(defn minor-column-layout
  [title technology desc]
    (div
      (h4 title (small technology))
      (p desc)))

;;;;;;;;;;;;;;;;;;;;
;; Layouts        ;;
;;;;;;;;;;;;;;;;;;;;

(defn category-appender [category]
  (if category
    (str "TECHNOLOGIES: " category)
    category))

(defn minor-line-builder [institution]
  (let [{:keys [title technology desc]} institution]
    (minor-column-layout title (category-appender technology) desc)))

(defn line-builder [institution & [etc]]
  (let [{:keys [subtitle location title date synopsis desc technology]} institution]
    (single-column-layout subtitle location title date synopsis desc (category-appender technology) etc)))

(defn achievement-list-builder [achievements html-out template]
  (if (seq achievements)
    (achievement-list-builder (rest achievements)
                              (str html-out (template (first achievements)))
                              template)
    html-out))

(defn two-col-list
  "I take a list and build out an entire column"
  [list-column template]
  (div {:class "two-col-list"}
       (ul {:class "list-unstyled"}
        (achievement-list-builder list-column  "" template))))

(defn section-header [header-name & additional-class]
  (div {:class "col-xs-12"}
       (h4 {:class (str "text-center section-header " (first additional-class))} header-name)))

(def resume-footer
  (fn [] (footer {:class "footer"}
                 (div {:class "container"}
                      (p "Made With Clojure &nbsp;|&nbsp; See the Source Code: "
                         (a {:href "http://bit.ly/2bWWDHc"} "http://bit.ly/2bWWDHc"))))))

;;;;;;;;;;;;;;;;;;;;
;; Layout: Header ;;
;;;;;;;;;;;;;;;;;;;;

(def resume-header
  (fn []
    (header {:class "row"}
            (div {:class "col-sm-6"}
                 (h1 (bio/biography :name))
                 (h2 (bio/biography :job-title)))
            (div {:class "col-sm-6"}
                 (div {:class "contact-info"}
                      (table {:class "table"}
                             (tr (td (span {:class "glyphicon glyphicon-envelope" :aria-hidden "true"}) "&nbsp" (bio/biography :email))
                                 (td (span {:class "glyphicon glyphicon-globe" :aria-hidden "true"}) "&nbsp;" (bio/biography :website)))
                             (tr (td (span {:class "glyphicon glyphicon-earphone" :aria-hidden "true"}) "&nbsp" (bio/biography :phone))
                                 (td (span {:class "glyphicon glyphicon-home" :aria-hidden "true"}) "&nbsp;" (bio/biography :city))))
                      )))))

;;;;;;;;;;;;;;;;;;;;
;; Layout: Awards ;;
;;;;;;;;;;;;;;;;;;;;

(defn award-layout [award]
  (li
   (if (= (award :highlight) "strong")
     (p {:class "highlight"} (strong (award :title)))
     (p (strong (award :title))))
   (p (award :org) "(" (award :type) ")")))

(def awards
  (fn [exhib] (let [award-list (exhib :awards)
               number-of-awards (count award-list)
               half (math/ceil (/ number-of-awards 2))
               html-awards ""]
           (div {:class "row"}
                (section-header "Awards")
                (two-col-list (take half award-list) award-layout)
                (two-col-list (drop half award-list) award-layout)))))

;;;;;;;;;;;;;;;;;;;;
;; 3 Talents      ;;
;;;;;;;;;;;;;;;;;;;;

(def talent-column (fn [talent]
                     (column-layout ((bio/talent (keyword talent)) :title)
                                    ((bio/talent (keyword talent)) :domain)
                                    ((bio/talent (keyword talent)) :desc))))

(def talent
  (fn [] (div {:class "row"}
              (talent-column "educator")
              (talent-column "programmer")
              (talent-column "creative"))))

(def bio-summary
  (fn []
    (div {:class "row"}
         (section-header "Summary")
         (div {:class "col-xs-12"}
              (p (bio/biography :summary))))))

;;;;;;;;;;;;;;;;;;;;
;; Experience     ;;
;;;;;;;;;;;;;;;;;;;;

(def experience
  (fn [proj employ]
    (let [jack-machine (project-finder "Jack and the Machine" (proj :projects) "title")
          borderless   (project-finder "Borderless" (proj :projects) "title")
          rhythm       (project-finder "The Rhythm of Time" (proj :projects) "title")
          distant      (project-finder "Distant Apologies" (proj :projects) "title")
          btf          (project-finder "Beyond the Frame" (employ :employers) "subtitle")
          penguin      (project-finder "Penguin Random House" (employ :employers) "subtitle")
          netgalley    (project-finder "NetGalley" (employ :employers) "subtitle")
          ef-sharp     (project-finder "F#" (employ :employers) "subtitle")]

      (div {:class "row"}
           (section-header "Selected Experience" "experience-section-header")

           (->> (clojure.core/map #(minor-line-builder %) [jack-machine rhythm distant])
                (reduce str)
                (div {:style "padding-left: 2.5em;"})
                (line-builder btf))

           (reduce str (clojure.core/map #(line-builder %) [penguin netgalley ef-sharp]))))))

;;;;;;;;;;;;;;;;;;;;
;; Academic       ;;
;;;;;;;;;;;;;;;;;;;;

;; Update database

(def academy
  (fn [exhib talk] (let [nu (project-finder "Northwestern University" (exhib :education) "subtitle")
               uni (project-finder "University of Northern Iowa" (exhib :education) "subtitle")
               ai (project-finder "Illinois Institute of Art" (talk :talks) "subtitle")]
           (div {:class "row"}
                (section-header "Selected Academic Experience")
                (column-layout (ai :subtitle) (ai :title) (ai :location))
                (column-layout (nu :subtitle) (nu :title) (nu :concentrations))
                (column-layout (uni :subtitle) (uni :title) (uni :concentrations))))))


(def academy-horiz
  (fn [exhib] (let [nu (project-finder "Northwestern University" (exhib :education) "subtitle")
               uni (project-finder "University of Northern Iowa" (exhib :education) "subtitle")]
           (div {:class "row"}
                (section-header "Education")
                (reduce str (clojure.core/map #(line-builder %) [nu uni]))
                ;; (single-column-layout (nu :org) (nu :concentrations) (nu :title) (nu :year))
                ;; (single-column-layout (uni :org) (uni :concentrations) (uni :title)  (uni :year))
                ))))

(def talks
  (fn [talk]
    (let [stevens   (project-finder "Stevens Institute of Technology" (talk :talks) "subtitle")
          ai        (project-finder "Illinois Institute of Art" (talk :talks) "subtitle")
          ccc       (project-finder "Columbia College Chicago" (talk :talks) "subtitle")
          iadt      (project-finder "International Academy of Design and Technology" (talk :talks) "subtitle")

          clj-conj  (project-finder "Aesthetics and Narrative" (talk :talks) "title")
          vcfmw     (project-finder "Accidentally Arming a Hacker Revolution" (talk :talks) "title")
          i-take    (project-finder "I T.A.K.E Unconference (Keynote), Bucharest, Romania" (talk :talks) "location")
          clj-brdg  (project-finder "Teacher: Advanced Track" (talk :talks) "title")
          modes     (project-finder "The Grammar of the Internet" (talk :talks) "title")
          nycdh     (project-finder "Strategies for Interactive and Immersive Dance" (talk :talks) "title")
          c-base    (project-finder "Harvesting Human Intelligence" (talk :talks) "title")
          pecha     (project-finder "Computers & Intimacy" (talk :talks) "title")]

      (div
       (div {:class "row"}
            (section-header "Research and Teaching Experience" "experience-section-header")
            (reduce str (clojure.core/map #(line-builder %) [stevens ai iadt ccc])))
       (div {:class "row"}
            (div {:class "col-xs-12 job"}
                (section-header "Related Public Speaking Experience")
                (two-col-list [clj-conj vcfmw i-take clj-brdg] speaking-layout)
                (two-col-list [modes nycdh c-base pecha] speaking-layout)))
       ))))


;;;;;;;;;;;;;;;;;;;;
;; Publications   ;;
;;;;;;;;;;;;;;;;;;;;

(defn pub-layout [pub]
  (li
   (p (strong (pub :title)))
   (p (pub :publication))))

(def publications
  (fn [exhib] (let [pub-list (exhib :publications)
               html-pub ""
               first-2-pubs (take 2 pub-list)
               second-2-pubs (take 2 (drop 2 pub-list))]
           (div {:class "row"}
                (section-header "Selected Publications")
                (two-col-list first-2-pubs pub-layout)
                (two-col-list second-2-pubs pub-layout)))))

;;;;;;;;;;;;;;;;;;;;
;; Sub-Layouts    ;;
;;;;;;;;;;;;;;;;;;;;

;; Programming

(defn programming-sub-layout [data-set]
  (div
   ;;(talent) ;; alternative header: 3 columns listing talents
   (bio-summary) ;; summary header
   (hr-)

   (experience (data-set :projects) (data-set :employment))
   (hr-)

   (talks (data-set :talks))
   (hr- )

   ;; (awards (data-set :exhibitions))
   ;; (hr-)

   (publications (data-set :exhibitions))
   (hr-)

   ;; 3 columns of academic experience
   (academy (data-set :exhibitions) (data-set :talks))

   ))

;; Teaching

(defn teaching-sub-layout [data-set]
  (div

   (talent) ;; alternative header: 3 columns listing talents
   ;; (bio-summary) ;; summary header
   (hr-)

   (talks (data-set :talks))

   (div {:class "page-breaker"})

   (publications (data-set :exhibitions))
   (hr-)

   ;;(experience (data-set :projects) (data-set :employment))
   ;;(hr-)

   (awards (data-set :exhibitions))
   (hr-)

   (academy-horiz (data-set :exhibitions))
   ))

;;;;;;;;;;;;;;;;;;;;
;; Main Layout    ;;
;;;;;;;;;;;;;;;;;;;;

(defn -main
  [& args]

  (let [data-set (get-data)]
  (println
   (html
    (head (title {:style "font-family:'Courier';"} "D. Schm&uuml;dde: Resume")
          (link {:rel "stylesheet" :type "text/css" :href "css/bootstrap.min.css" :media "all"})
          (link {:rel "stylesheet" :href "css/styles.css"}))
    (body
     (div {:class "container" :id "inner"}
          ;; header: name, title, phone number, etc...
          (resume-header)

          ;; document body
          (div {:id "bd"}
               (programming-sub-layout data-set)
               ;;(teaching-sub-layout data-set)
               ))

     (resume-footer))))))
