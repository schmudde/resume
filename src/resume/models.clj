(ns resume.models)

(def projects-db
  {:projects
   [{:title "Jack and the Machine"
     :synopsis "Once known as #148445 within the Third Reich, Jack Tramiel rose from the Nazi concentration camps and built the information age's great equalizer: the world's first 'computer for the masses, not the classes.'"
     :status "In Production"
     :link-status true
     :link "http://www.jackandthemachine.com"
     :tags [{:tag "documentary"} {:tag "interactive"}]
     :media "https://player.vimeo.com/video/74458487"
     :video true
     :image false
     :position "oneBox"
     :technology "Clojure, PHP, JavaScript"
     :desc "Interactive documentary based on the life and times of Jack Tramiel, the founder of Commodore Business Machines. Awarded a three month residency where prototypes were built and video/audio assets were compiled."}
    {:title "Borderless"
     :synopsis "&ldquo;Borderless&rdquo; is an exploration of unconscious interactions among strangers. The piece examines the way we shape and are shaped by our surroundings, creating invisible movement within society's social fabric. &ldquo;Borderless&rdquo; tests the idea that by recreating unconscious interactions within a tactile and visible medium, we can unveil nuances in social behavior and response. A collaboration with artist Kim Burgas."
     :status "Performance Premier: Music Hall of Williamsburg 2015 (USA); Exhibition Premier: Schusev State Museum of Architecture Now&After Festival 2015 (World)"
     :link-status true
     :link "http://www.beyondthefra.me/borderless.html"
     :tags [{:tag "installation"}]
     :media "https://player.vimeo.com/video/119274499"
     :video true
     :image false
     :position "twoBox"
     :technology "Clojure, Overtone, Kinect, TSPS, Blackmagic Cinema Camera, portable projection"
     :desc "The experience is a combination of traditional projection onto semi-transparent organza sheets and soundscapes generated in real-time. Overtone, an open source audio environment in Clojure, receives room information from a Kinect and reacts accordingly."}
    {:title "The Rhythm of Time"
     :synopsis "&ldquo;The Rhythm of Time&rdquo; weaves past and future in musical counterpoint as it explores the life and times of an enigmatic instrumentalist hailing from an unexpected place."
     :status "New Release"
     :link-status true
     :link "http://rhythmoftime.xyz"
     :tags [{:tag "documentary"}{:tag "interactive"}]
     :media "rhythm-16x9.jpg"
     :video false
     :image true
     :position "twoBox"
     :technology "ClojureScript, FigWheel, Quil, JavaScript, Canon 7D, Final Cut X, Logic X"
     :desc "&ldquo;The Rhythm of Time&rdquo; is a poetic documentary on the life and times of renowned synthesist Suit & Tie Guy. An interactive element familiarizes the audience with the tools of electronic music sequencing, a key theme of the film."}
    {:title "Distant Apologies"
     :synopsis "The closer you are, the more difficult it is to say, &quot;I'm sorry.&quot;"
     :status "New Release"
     :link-status true
     :link "http://schmud.de/distant"
     :tags [{:tag "narrative"}{:tag "interactive"}]
     :media "https://player.vimeo.com/video/160771838"
     :video true
     :image false
     :position "twoBox"
     :technology "Clojure, Leiningen, Tomcat, Twitter API, Super 8 Kodak Film, Final Cut X, Logic X"
     :desc "&ldquo;Distant Apologies&rdquo; mixes rich NASA archival with a short narrative shot on Super 8 film. The experience begins on a website that streams recent apologies broadcast by individuals around the world on Twitter."}
    {:title "The Assassination of Chicago's Mayor"
     :synopsis "A 19th-century newspaper peddler named Patrick Prendergast has delusions of greatness. Evidently slighted by the mayor, this self-anointed messenger prepares for a glorious rise from ignored lobbyist to influential icon. Prendergast sees himself as a martyr, serving God with a noble purpose. The reality is far different."
     :status "Festival Premier: Hollyshorts 2012 (USA), Aesthetica Film Festival 2012 (World); Broadcast Premier: ShortsHD Channel - Dish Network, DirecTV, AT&T; Internet: IndieFlix"
     :link-status true
     :link "https://indieflix.com/indie-films/the-assassination-of-chicagos-mayor-34358/"
     :tags [{:tag "narrative"}]
     :media "https://player.vimeo.com/video/30803843"
     :video true
     :image false
     :position "twoBox"}
    {:title "A Simple Reminder"
     :synopsis "The essence of memory - a possessor, a liberator."
     :status "Festival Premier: Filmstock Film Festival 2013"
     :link-status false
     :link ""
     :tags [{:tag "narrative"}]
     :media "https://player.vimeo.com/video/46622645"
     :video true
     :image false
     :position "twoBox"}
    {:title "The Invisible Majority"
     :synopsis "Every single day, people volunteer, search for cures, and engineer leaps in efficiency. Unfortunately, media is often usurped by fearmongers and fundamentalists who spread false conceptions as facts. It is up to us, the majority of us who do good day-in and day-out, to guide the larger narrative."
     :status "Online"
     :link-status false
     :link ""
     :tags [{:tag "documentary"}]
     :media "https://player.vimeo.com/video/52299617"
     :video true
     :image false
     :position "twoBox"}
    {:title "Refuge"
     :synopsis "Grant is trapped with his conscious on the long, lonely drive across the American Midwest. As the endless road stretches on, he is confronted by those he has loved and those he has hurt. &ldquo;Refuge&rdquo; is a metaphysical journey to truth, quickly turning the open road into a claustrophobic asylum of haunting self-examination."
     :status "Festival Premier: Chicago International Film Festival 2010; Broadcast Premier: ShortsHD Channel - Dish Network, DirecTV, AT&T"
     :link-status false
     :link ""
     :tags [{:tag "narrative"}]
     :media "https://player.vimeo.com/video/8247688"
     :video true
     :image false
     :position "twoBox"}
    {:title "Schm&uuml;dde: A Retrospective"
     :synopsis "A Selection of Music Written Between 1998 and 2012."
     :status "Online"
     :link-status true
     :link "https://soundcloud.com/schmudde/sets/schmudde-a-retrospective"
     :tags [{:tag "sound"}]
     :media "https://w.soundcloud.com/player/?url=https%3A//api.soundcloud.com/playlists/23618316&amp;auto_play=false&amp;hide_related=false&amp;show_comments=true&amp;show_user=true&amp;show_reposts=false&amp;visual=true"
     :video true
     :image false
     :position "twoBox"}
    {:title "Schm&uuml;dde: Film Music"
     :synopsis "Music for Films"
     :status "Online"
     :link-status true
     :link "https://soundcloud.com/schmudde/sets/film-scores"
     :tags [{:tag "sound"}]
     :media "https://w.soundcloud.com/player/?url=https%3A//api.soundcloud.com/playlists/144853848&amp;auto_play=false&amp;hide_related=false&amp;show_comments=true&amp;show_user=true&amp;show_reposts=false&amp;visual=true"
     :video true
     :image false
     :position "twoBox"}
    {:title "Elements: Spring to Summer to Autumn 2011"
     :synopsis "Human-kind is essential. Without us, who would admire the flower? Who would discover new worlds and pay tribute to old ones?"
     :status "Festival Premier: Chicago Filmmakers Short Story Showcase 2011"
     :link-status false
     :link ""
     :tags [{:tag "documentary"}]
     :media "https://player.vimeo.com/video/34661968"
     :video true
     :image false
     :position "twoBox"}
    {:title "Elements: Winter 2011"
     :synopsis "The four elements in winter."
     :status "Online"
     :link-status false
     :link ""
     :tags [{:tag "documentary"}]
     :media "https://player.vimeo.com/video/21351915"
     :video true
     :image false
     :position "twoBox"}
    {:title "Elements: Fall 2010"
     :synopsis "The beginning of fall as told through the elements."
     :status "Online"
     :link-status false
     :link ""
     :tags [{:tag "documentary"}]
     :media "https://player.vimeo.com/video/15209878"
     :video true
     :image false
     :position "twoBox"}
    {:title "Lucid Dream Ensemble - 3. Red (2003)"
     :synopsis "A live digital arts performance ensemble. Performers: Virgil Moorefield: Founder, V-Drums; D. Schm&uuml;dde: Virtual Analog Synthesizers, Analogue Sequencing; Nathan Woleck: Max/MSP, Granular Synthesis; Jeff Weeter: Jitter Visuals."
     :status "Performance Premier: SEAMUS National Conference; Noted Performance: Museum of Contemporary Art in Chicago"
     :link-status false
     :link ""
     :tags [{:tag "documentary"}]
     :media "https://player.vimeo.com/video/84801854"
     :video true
     :image false
     :position "twoBox"}
    {:title "Reflections"
     :synopsis "&ldquo;Continuing to explore human truths in a vox populi format, Ginsburg trains his camera on Chicago citizens to ask &#39;What is freedom?&#39; The answer is always surprising. Like their previous films &#39;Aijo&#39; and &#39;The Year of the Rooster,&#39; narrative elements add suspense and a poignant dimension to the interviews featured on screen.&rdquo; -- Todd Lillethum, August 2009, Programming Director, Chicago Filmmakers"
     :status "Online"
     :link-status true
     :link "https://www.youtube.com/watch?v=5yB2LWTfSYs"
     :tags [{:tag "documentary"}]
     :media "reflections.jpg"
     :video false
     :image true
     :position "twoBox"}]
   :page-title "Projects"
   :tags [{:tag "documentary"}
          {:tag "narrative"}
          {:tag "installation"}
          {:tag "interactive"}
          {:tag "sound"}]
   })

(def employment-db
  {:employers
   [{:employer "Beyond the Frame"
     :date "1/2005 - Present"
     :title "Creative Technologist"
     :technology "Clojure, Apache, Tomcat, Digital and 35mm Cameras, Final Cut X, Logic X, Asana, GitHub"
     :desc "As the company's founder, I interface with clients, manage projects, and generate media assets while acting as the lead developer and creative director."
     }
    {:employer "Penguin Random House"
     :date "10/2015 - 4/2016"
     :title "Developer"
     :technology "PHP, JavaScript, Wordpress"
     :desc "Developer for independent author sites and initiatives at the Crown Publishing Group subsidiary of Random House. Sites include Hogarth Shakespeare, Arianna Huffington, and Carol Burnett."
     }
    {:employer "NetGalley"
     :date "12/2013 - 4/2015"
     :title "Front End Developer"
     :technology "Symfony 2, Symfony 1, PHP, JavaScript, JQuery, Less, Doctrine, GitHub."
     :desc "Front end developer for publishing website with 40,000+ members and 100k+ uniques/month. The team used agile methodologies to deploy new releases every two weeks."
     }
    {:employer "F#"
     :date "9/2012 - 12/2012"
     :title "Digital Project Manager/Digital Producer"
     :technology "Jira, Spotify API"
     :desc "I managed the creation of online advertising for clients such as Universal Pictures, Chevrolet, Philips, Gap, and Spotify.  The process included ideation, wireframes, design, agile development, content management solutions, and launch.  The agile-based production process spanned three continents making cultural awareness and timezone management key."
     }]
   })

(def talks-db
  {:talks
   [{:location "C-Base: Home of the Chaos Computer Club in Berlin, Germany"
     :date "July 2015"
     :title "Harvesting Human Intelligence"
     :subtitle "Reframing the Surveillance Discourse"
     :synopsis "The power of large government and corporate surveillance systems feel like an intractable part of our everyday digital lifestyle. However, the raw intelligence of centralized computer systems pale in comparison to decentralized personal computers, each augmented by a human actor. This talk examines how the PC threatens traditional power structures and how those systems have responded."
     :link-status true
     :link "https://digitalegesellschaft.de/2015/07/npa039/"
     :link-name "Digitalegesellschaft: Netzpolitischen Abend #38"
     :media "https://player.vimeo.com/video/132857801"
     :video true
     :image false
     :row-start "<div class='row'>"
     :row-end ""
    }
    {:location "Pecha Kucha, Berlin, Germany"
     :date "July 2015"
     :title "Computers & Intimacy"
     :subtitle "The Story of the Largest Machines and the Smallest Details"
     :synopsis "Our thoughts are the material of the Internet. In the midst of ever-evolving man/machine integration, I’ll use declassified documents to show how politicians have used computers to monitor these thoughts in the past, and suggest new ways of thinking about intimacy and computers in the future."
     :link-status true
     :link "http://pechakucha.de/berlin/player/?event=PK_39_1&position=03&presenter=D.%20Schm%C3%BCdde&presentation=Computers%20&%20Intimacy%20%E2%80%93%20The%20Story%20of%20the%20Largest%20Machines%20and%20the%20Smallest%20Details"
     :link-name "Pecha Kucha Slides &amp; Audio"
     :media "intimacy.jpg"
     :video false
     :image true
     :row-start ""
     :row-end ""
    }
    {:location "Department of Digital Film and Video, Chicago, IL"
     :date "January 2006 - January 2012"
     :title "Associate Professor"
     :subtitle "Illinois Institute of Art"
     :synopsis "Courses: Sound Design for Film <span class='small'>(DFV 365)</span>, Survey of Film <span class='small'>(DFV 100)</span>, Digital Filmmaking Portfolio Preparation I <span class='small'>(DFV 330)</span>, Digital Filmmaking Portfolio Preparation II <span class='small'>(DFV 430)</span>, Fundamentals of Audio <span class='small'>(DFV 120)</span>, Survey of the Music Industry <span class='small'>(AUD 100)</span>, Music Theory <span class='small'>(AUD 201)</span>"
     :link-status false
     :link ""
     :link-name ""
     :media "ai.png"
     :video false
     :image true
     :row-start ""
     :row-end "</div> <!-- /row -->"
    }
    {:location "MFA: Music Composition for the Screen, Chicago, IL"
     :date "October 2009"
     :title "Hybrid Scores and Atonality"
     :subtitle "Columbia College Chicago"
     :synopsis "<blockquote><p>It may be that in order to make the cinematic image sound authentically, in its full diapason, music has to be abandoned.</p><footer>Andrei Tarkovsky</footer></blockquote>A master class that examines the intersection between sound design and traditional notions of composing music for the screen."
     :link-status true
     :link "http://beyondthefra-me.tumblr.com/post/5545059622"
     :link-name "D. Schm&uuml;dde: &quot;Composing for <em>The Assassination of Chicago&#39;s Mayor</em>&quot;"
     :media "ccc.jpg"
     :video false
     :image true
     :row-start "<div class='row'>"
     :row-end ""
    }
    {:location "Video and Animation Department, Chicago, IL"
     :date "January 2005 - June 2007"
     :title "Adjunct Faculty"
     :subtitle "International Academy of Design and Technology"
     :synopsis "Courses: Directing and Producing, Application Design, Digital Production I, Advanced Post Production, Digital Video Editing, Audio Production, Digital Audio Editing, Senior Project for Visual Communications, Career Portfolio Development for Visual Communications, Independent Study"
     :link-status false
     :link ""
     :link-name ""
     :media "iadt.png"
     :video false
     :image true
     :row-start ""
     :row-end ""
    }
    {:location "Midwest Independent Film Festival, Chicago, IL"
     :date "September 2009"
     :title "Moderator"
     :subtitle "&quot;Low Budget Filmmaking&quot;"
     :synopsis "A discussion of low budget filmmaking techniques for independent filmmakers."
     :link-status true
     :link "http://beyondthefra-me.tumblr.com/post/53206393777"
     :link-name "D. Schm&uuml;dde: &quot;The Magic of M&#233;li&#232s: Low Budget Filmmaking&quot;"
     :media "miff.png"
     :video false
     :image true
     :row-start ""
     :row-end "</div> <!-- /row -->"
    }

    {:location "Chicago Actors Casting Summit, Chicago, IL"
     :date "July 2008 &amp; July 2009"
     :title "Panelist"
     :subtitle "&quot;Casting in Student Films&quot;"
     :synopsis "A discussion with actors on the benefits and concerns associated with working on student-run film sets."
     :link-status true
     :link "http://beyondthefra-me.tumblr.com/post/28430696505"
     :link-name "D. Schm&uuml;dde: &quot;Reader Question: Student/Actor Expectations&quot;"
     :media ""
     :video false
     :image false
     :row-start "<div class='row'>"
     :row-end "</div> <!-- /row -->"
    }]
   :page-title "Talks"})

(def exhibitions-db
  {:exhibitions
   [{:title "Third Shift @ Third Space Gallery"
     :place "Installation, Saint John, N.B., Canada, 2016"
     :piece "Borderless"
    }
    {:title "FUSE @ Livestream Public"
     :place "Performance, New York City, 2015"
     :piece "Borderless"
    }
    {:title "Music Hall of Williamsburg"
     :place "Performance, New York City, 2015"
     :piece "Borderless"
     :highlight "strong"
    }
    {:title "ArtStars* Bits & Beats"
     :place "Berlin 2015"
     :piece "Jack and the Machine"
    }
    {:title "310 XNRD"
     :place "2 Day Workshop & Installation, London, 2015"
     :piece "Borderless"
    }
    {:title "Schusev State Museum of Architecture Now&After Festival"
     :place "Installation, Moscow, 2015"
     :piece "Borderless"
     :highlight "strong"
    }
    {:title "IndieWorks"
     :place "New York City, 2015"
     :piece "The Assassination of Chicago's Mayor"
    }
    {:title "IFP Made in NY Media Center"
     :place "Installation, New York City, 2014"
     :piece "Borderless"
    }
    {:title "Red Hook Film Festival"
     :place "New York City, 2014"
     :piece "The Assassination of Chicago’s Mayor"
    }
    {:title "ShortsHD Channel - Dish Network, DirecTV, AT&T"
     :place "Broadcast, 2014"
     :piece "The Assassination of Chicago’s Mayor"
     :highlight "strong"
    }
    {:title "Filmstock Film Festival"
     :place "Utah, 2013"
     :piece "A Simple Reminder"
    }
    {:title "Hollyshorts"
     :place "New York City, 2013"
     :piece "The Assassination of Chicago's Mayor"
     :highlight "strong"
    }
    {:title "Filmstock Film Festival"
     :place "New Mexico, 2013"
     :piece "Refuge"
    }
    {:title "IndieFlix"
     :place "Internet Distribution, 2014"
     :piece "The Assassination of Chicago's Mayor"
     :highlight "strong"
    }
    {:title "Beneath the Earth Film Festival"
     :place "Internet Distribution, 2012"
     :piece "Refuge"
    }
    {:title "Filmstock Film Festival"
     :place "Arizona, 2012"
     :piece "The Assassination of Chicago's Mayor"
    }
    {:title "Filmstock Film Festival"
     :place "Arizona, 2012"
     :piece "Refuge"
    }
    {:title "Aesthetica Film Festival"
     :place "York, UK, 2013"
     :piece "The Assassination of Chicago's Mayor"
     :highlight "strong"
    }
    {:title "Hollyshorts"
     :place "Los Angeles, 2012"
     :piece "The Assassination of Chicago's Mayor"
    }
    {:title "MoFest"
     :place "Chicago, 2012"
     :piece "Elements • Spring to Summer to Autumn 2011"
    }
    {:title "Chicago Filmmakers Short Story Showcase"
     :place "Chicago, 2012"
     :piece "Elements • Spring to Summer to Autumn 2011"
    }
    {:title "Moving Pictures Film Festival"
     :place "Quincy, Illinois, 2012"
     :piece "Refuge"
    }
    {:title "ShortsHD Channel - Dish Network, DirecTV, AT&T"
     :place "Broadcast, 2011"
     :piece "Refuge"
     :highlight "strong"
    }
    {:title "Chicago Filmmakers Short Story Film Showcase"
     :place "Chicago, 2011"
     :piece "Refuge"
    }
    {:title "Chicago International Film Festival"
     :place "Chicago, 2010"
     :piece "Refuge"
     :highlight "strong"
    }
    {:title "The New Art Film Festival"
     :place "Champaign-Urbana, Illinois, 2010"
     :piece "I am Concrete"
    }
    {:title "Chicago Filmmakers"
     :place "Chicago, 2008"
     :piece "Aijo"
    }
    {:title "Georgetown Film Fest"
     :place "Washington DC, 2006"
     :piece "Aijo"
    }
    {:title "S.N.O.B Film Festival"
     :place "Boston, 2006"
     :piece "Aijo"
    }
    {:title "River’s Edge International Film Festival"
     :place "Paducah, Kentucky, 2005"
     :piece "Year of the Rooster"
    }
    {:title "Block Museum"
     :place "Evanston, Illinois, 11/2003"
     :piece "Lucid Dream Ensemble"
    }
    {:title "Block Museum"
     :place "Evanston, Illinois, 4/2003"
     :piece "Lucid Dream Ensemble"
    }
    {:title "Chicago Museum of Contemporary Art Version Festival"
     :place "Chicago 2003"
     :piece "Lucid Dream Ensemble"
     :highlight "strong"
    }
    {:title "SEAMUS National Conference"
     :place "Tempe, Arizona, 2003"
     :piece "Lucid Dream Ensemble"
    }]
   :awards
    [{:title "Berlin/New York 2015 Media Residency Program"
      :type "New Media Production Grant"
      :org "Startup Germany/Medienboard"
      :highlight "strong"
     }
     {:title "POV Hackathon 9"
      :type "Competitive Selection"
      :org "PBS"
      :highlight "strong"
     }
     {:title "POV Hackathon 7"
      :type "Competitive Selection"
      :org "PBS"
     }
     {:title "Audience Choice Award, Best Director: &ldquo;The Assassination of Chicago&#39;s Mayor&rdquo;"
      :type "Film Festival Award"
      :org "Red Hook Film Festival 2014"
      :highlight "strong"
     }
     {:title "Audience Choice Award: &ldquo;A Simple Reminder&rdquo;"
      :type "Film Festival Award"
      :org "Digipops Community Film Festival 2014"
     }
     {:title "Tribeca Hacks Winter 2013"
      :type "Competitive Selection"
      :org "Tribeca Film Institute"
     }
     {:title "Best Director, Best of Fest: &ldquo;Refuge&rdquo;"
      :type "Film Festival Award"
      :org "Filmstock Film Festival 2012"
      :highlight "strong"
     }
     {:title "Best Actor: &ldquo;The Assassination of Chicago&#39;s Mayor&rdquo;"
      :type "Film Festival Award"
      :org "Filmstock Film Festival 2012"
     }
     {:title "Best Editing, Best Acting: &ldquo;Refuge&rdquo;"
      :type "Film Festival Award"
      :org "Beneath the Earth Film Festival 2012"
      :highlight "strong"
     }
     {:title "Educator of the Year Nomination"
      :type "Professional Distinction"
      :org "International Academy of Design and Technology 2005"
     }
     {:title "Program for the Study of the Imagination 2003"
      :type "Composition Grant"
      :org "The Program in the Study of Imagination at Northwestern University"
     }
     {:title "Interactive Award - The One Show 1998"
      :type "Competitive Showcase (Bronze Placement)"
      :org "One Club"
     }]
   :affiliations
    [{:org "The FilmShop: a New York Collective of Independent Filmmakers"
      :title "Member"
     }
     {:org "Bushwick Film Festival 2013 &amp; 2014"
      :title "New Media Advisor"
     }
     {:org "Himalaya Project: Education and Healthcare in Dolpo, Nepal"
      :title "Volunteer Media Director and Producer"
     }
     {:org "&lt;StoryCode&gt;: an Immersive Media Meetup"
      :title "Videography Volunteer"
     }
     {:org "The Script Lab"
      :title "Screenwriting Analysis for &ldquo;From Script to Screen&rdquo;"
     }]
   :education
    [{:org "Northwestern University"
      :title "Masters in Music (Magna Cum Laude)"
      :concentrations "Electroacoustic Performance, Composition, Intermedia Studies"
      :year "2004"
     }
     {:org "University of Northern Iowa"
      :title "Bachelor of Arts in Computer Science"
      :concentrations "Artificial Intelligence, Creative Computing and the Visual Arts"
      :year "2001"
     }
     {:org "DID Deutsch-Institut, Berlin, Germany"
      :title "Language Intensive"
      :concentrations "German"
      :year "2005"
     }]
   :publications
    [{:title "Our Lives, Encoded"
      :publication "Broken Toilets and shared by <a href='http://epodharvard.tumblr.com/post/146951238165/five-favorites-7516' target='_blank'>Evidence For Policy Design at Harvard University</a>"
      :link "http://brokentoilets.org/article/year-twenty-megabytes/"
      }
     {:title "The Computer Revolution Has Yet to Happen"
      :publication "Endless Magazine/The Absurdist"
      :link "https://medium.com/absurdist/the-computer-revolution-has-yet-to-happen-f1dbf983d477"
      }
     {:title "Beyond the Frame"
      :publication "A Weekly Weblog With 57,000 Subscribers"
      :link "http://beyondthefra-me.tumblr.com"
      }
     {:title "A Brief History of Machine Personalities - The Character &amp; Ephemera From a Century of Machine Thinking"
      :publication "Self-Published"
      :link "https://medium.com/@dschmudde/a-brief-history-of-machine-personalities-f7116c85c2ed"
      }
     {:title "From Script to Screen: &quot;Five Easy Pieces&quot; - Bobby &amp; Rayette"
      :publication "The Script Lab"
      :link "http://thescriptlab.com/screenwriting-101/screenplay/from-script-to-screen/1228-five-easy-pieces-1970-bobby-a-rayette"
      }
     {:title "From Script to Screen: &quot;The Matrix&quot; - Lobby Scene"
      :publication "The Script Lab"
      :link "http://thescriptlab.com/screenwriting-101/screenplay/from-script-to-screen/1231-the-matrix-1999-lobby-scene"
      }]
   :page-title "Curriculum Vitae"})

(def database
  {:talks talks-db
   :projects projects-db
   :exhibitions exhibitions-db
})
