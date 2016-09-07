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
  (load-string (slurp "https://raw.githubusercontent.com/schmudde/schmud-de/master/src/schmud_de/models.clj")))

;; (def matcher (re-matcher #"(?s)\(def.*?\}+?\)" database))


;;;;;;;;;;;;;;;;;;;;
;; General Layout ;;
;;;;;;;;;;;;;;;;;;;;

(defn project-finder [search-term db keyword-search]
  (if (= ((first db) (keyword keyword-search)) search-term)
    (first db)
    (project-finder search-term (rest db) keyword-search)))

(defn achievement-list-builder [achievements html-out template]
  (if (seq achievements)
    (achievement-list-builder (rest achievements)
                              (str html-out (template (first achievements)))
                              template)
    html-out))

(defn column-layout [talent title description]
  (div {:class "col-xs-4"}
       (h2 talent)
       (h3 title)
       (p description)))

(defn two-col-list [list-column template]
  (div {:class "two-col-list"}
       (ul {:class "list-unstyled"}
        (achievement-list-builder list-column  "" template))))

(defn section-header [header-name & additional-class]
  (div {:class "col-xs-12"}
       (h4 {:class (str "text-center section-header " (first additional-class))} header-name)))

(def resume-footer
  (fn [] (footer {:class "footer"}
                 (div {:class "container"}
                      (p "Made With Clojure")))))

;;;;;;;;;;;;;;;;;;;;
;; Header         ;;
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
;; 3 Talents      ;;
;;;;;;;;;;;;;;;;;;;;

(def talent-column (fn [talent]
                     (column-layout ((bio/talent (keyword talent)) :title)
                                    ((bio/talent (keyword talent)) :domain)
                                    ((bio/talent (keyword talent)) :desc))))

(def talent
  (fn [] (div {:class "row"}
              (talent-column "creative")
              (talent-column "programmer")
              (talent-column "educator"))))

;;;;;;;;;;;;;;;;;;;;
;; Experience     ;;
;;;;;;;;;;;;;;;;;;;;

(def beyond-the-frame
  (fn [title technology description] (div {:style "padding-left: 2.5em;"}
                                      (h4 title (small technology))
                                      (p description))))

(defn desc-itemizer [matcher-object points]
  "I iterate through a java.util.regex.Matcher.find() object and turn each returned string into an HTML list item."
  (let [single-point (re-find matcher-object)]
    (if (some? single-point)
      (desc-itemizer matcher-object (str points (li {:class "qualification"} single-point)))
      points)))

(defn desc-layout [description]
  "regex to find the semicolon"
  (ul {:class "qualifications"} (desc-itemizer (re-matcher #"\w[^;]+" description) "")))

(defn experience-layout [employer]
  (div {:class "col-xs-12 job"}
       (h2 (employer :employer) (small {:class "pull-right"} (employer :date)))
       (h3 (employer :title) (small (employer :technology)))
       (desc-layout (employer :desc))))

(def experience
  (fn [proj employ] (let [jack-machine (project-finder "Jack and the Machine" (proj :projects) "title")
               borderless   (project-finder "Borderless" (proj :projects) "title")
               rhythm       (project-finder "The Rhythm of Time" (proj :projects) "title")
               distant      (project-finder "Distant Apologies" (proj :projects) "title")
               btf          (project-finder "Beyond the Frame" (employ :employers) "employer")
               penguin      (project-finder "Penguin Random House" (employ :employers) "employer")
               netgalley    (project-finder "NetGalley" (employ :employers) "employer")
               ef-sharp     (project-finder "F#" (employ :employers) "employer")]

           (div {:class "row"}
                (section-header "Selected Experience" "experience-section-header")
                (p {:class "text-center section-header"} "Technologies I Used in Red")
                (div {:class "col-xs-12 job"}
                     (h2 (btf :employer) (small {:class "pull-right"} (btf :date)))
                     (h3 (btf :title) (small (btf :technology)))
                     (desc-layout (btf :desc))
                     (beyond-the-frame (jack-machine :title) (jack-machine :technology) (jack-machine :desc))
                     (beyond-the-frame (borderless :title) (borderless :technology) (borderless :desc))
                     (beyond-the-frame (rhythm :title) (rhythm :technology) (rhythm :desc))
                     (beyond-the-frame (distant :title) (distant :technology) (distant :desc)))
                (experience-layout penguin)
                (experience-layout netgalley)
                (experience-layout ef-sharp)))))

;;;;;;;;;;;;;;;;;;;;
;; Academic       ;;
;;;;;;;;;;;;;;;;;;;;

;; Update database

(def academy
  (fn [exhib talk] (let [nu (project-finder "Northwestern University" (exhib :education) "org")
               uni (project-finder "University of Northern Iowa" (exhib :education) "org")
               ai (project-finder "Illinois Institute of Art" (talk :talks) "subtitle")]
           (div {:class "row"}
                (section-header "Selected Academic Experience")
                (column-layout (ai :subtitle) (ai :title) (ai :location))
                (column-layout (nu :org) (nu :title) (nu :concentrations))
                (column-layout (uni :org) (uni :title) (uni :concentrations))))))

;;;;;;;;;;;;;;;;;;;;
;; Awards         ;;
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
               ;; 3 columns listing talents
               (talent)
               (hr-)

               (experience (data-set :projects) (data-set :employment))
               (hr-)

               (awards (data-set :exhibitions))
               (hr-)

               (publications (data-set :exhibitions))
               (hr-)

               ;; 3 columns of academic experience
               (academy (data-set :exhibitions) (data-set :talks))))

     (resume-footer))))))
