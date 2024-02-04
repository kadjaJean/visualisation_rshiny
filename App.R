library(shiny)
library(shinyauthr)
library(shinydashboard)
library(tidyverse)
library(gridExtra)
library(grid)
library(wordcloud)
library(tm)
library(hrbrthemes)
library(stringr)

data <- readRDS("data.rds")
df_sep1 <- readRDS("df_sep1.rds")
df_sep <- readRDS("df_sep.rds")
df<-readRDS("dt1.rds")
# dataframe that holds usernames, passwords and other user data
user_base <- tibble::tibble(
  user = c("kadja_assagou_angie"),
  password = sapply(c("1234"), sodium::password_store),
  permissions = c("admin"),
  name = c("Kadja")
)

ui <- fluidPage(
  # Se déconnecter button
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  
  # Centrer le div de connexion
  tags$style(HTML("
    #login {
      position: fixed;
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%);
    }
    #login .btn-primary {
      background-color: #007BFF;
      border-color: #007BFF;
    }
    #login .btn-primary:hover {
      background-color: #0056b3;
      border-color: #0056b3;
    }
  ")),
  
  # Section de connexion
  shinyauthr::loginUI(id = "login", title = tags$h2("Connexion", style = "color:blue;text-align: center")),
  
  # Contenu principal du tableau de bord
  uiOutput("dashboard_content")
)

server <- function(input, output, session) {
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # Se déconnecter pour masquer
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  output$dashboard_content <- renderUI({
    # Afficher uniquement lorsque l'utilisateur est authentifié
    req(credentials()$user_auth)
    dashboardPage(skin = "blue",
    dashboardHeader(title = "My Dashboard"),
    
    dashboardSidebar(

      sidebarMenu(
        
        menuItem("Résumé Statistiques", tabName = "resume",icon = icon("chart-bar"),selected = FALSE),
        br(),
        menuItem("Connaissance de base", tabName = "connaissance_base",icon = icon("dashboard"),selected = FALSE),
        br(),
        menuItem("Connaissance générale", tabName = "connaissance_generale",icon = icon("dashboard"),selected = FALSE),
        br(),
        menuItem("Connaissance avancée ", tabName = "connaissance_avancee",icon = icon("dashboard"),selected = FALSE),
        br(),
        # menuItem("Connaissance Très avancée", tabName = "tres_avancee"),
        menuItem("Influence et perception", tabName = "influence_perception",icon = icon("dashboard"),selected = FALSE)
        
        
      )
    ),

    

    
    
    dashboardBody(

    tabItems(
      tabItem(tabName = "resume",
              #h2("Histogramme pour Connaissance de base"),
              #br(),
              fluidRow(
                valueBoxOutput("nb1"),
                valueBoxOutput("nb2"),
                valueBoxOutput("nb3"),
                
              ),
              fluidRow(
                valueBoxOutput("nb4"),
                valueBoxOutput("nb5"),
                valueBoxOutput("nb6"),
                
              ),
              
              fluidRow(
                width = 3,
                tags$div(
                  style = " font-size:25px; font-family:Source Sans Pro ;font-weight: bold; position: relative",
                  tags$p(textOutput("texte1"))  ))
              #verbatimTextOutput("code")
              
      ,
              fluidRow(
              width = 3,
              tags$div(
                style = " font-size:15px; font-family:Source Sans Pro ; position: relative",
                tags$p(textOutput("texte"))  )))
             #verbatimTextOutput("code")

      ,
      tabItem(tabName = "connaissance_base",
              #h2("Histogramme pour Connaissance de base"),
              #br(),
              fluidRow(
                column(6, plotOutput("plot1_connaissance_base")),
                column(6, plotOutput("plot2_connaissance_base"))
              ),
              fluidRow(
                column(6, plotOutput("plot3_connaissance_base")),
                column(6, plotOutput("plot4_connaissance_base"))
              )
      ),
      tabItem(tabName = "connaissance_generale",
              #h2("Histogramme pour Connaissance générale"),
              #plotOutput("plot_connaissance_generale")
              fluidRow(
                column(6, plotOutput("plot1_connaissance_generale")),
                column(6, plotOutput("plot2_connaissance_generale"))
              ),
              fluidRow(
                column(6, plotOutput("plot3_connaissance_generale")),
                column(6, plotOutput("plot4_connaissance_generale"))
              )
      ),
      tabItem(tabName = "connaissance_avancee",
              #h2("Histogramme pour Connaissance avancée"),
              #plotOutput("plot_connaissance_avancee")
              fluidRow(
                column(6, plotOutput("plot1_connaissance_avancee")),
                column(6, plotOutput("plot2_connaissance_avancee"))
              )
              #fluidRow(
                #column(6, plotOutput("plot3_connaissance_avancee")),
                #column(6, plotOutput("plot4_connaissance_avancee"))
              #)
              
      ),
   
      tabItem(tabName = "influence_perception",
              #h2("Histogramme pour Influence et perception"),
              #plotOutput("plot_influence_perception")
              #h2("Histogramme pour Connaissance avancée"),
              #plotOutput("plot_connaissance_avancee")
              fluidRow(
                column(6, plotOutput("plot1_influence_perception")),
                column(6, plotOutput("plot2_influence_perception"))
              ),
              fluidRow(
                column(6, plotOutput("plot3_influence_perception")),
                column(6, plotOutput("plot4_influence_perception"))
              )
      ))
    ))
  })
  
  
  
  output$nb1 <- renderValueBox({
    valueBox(
      value = dim(data)[1],
      subtitle = h2("Nombre de participants"),
      icon = icon("check"),
      color = "purple",  # Vert
      width = 4  # Ajustez la largeur si nécessaire
    )
  })
  
  output$nb2 <- renderValueBox({
    dt = data %>% filter(Pays_naiss_user != "aucune reponse")
    valueBox(
      value = length(unique(dt$Pays_naiss_user)),
      subtitle = h2("Nombre de pays participants"),
      icon = icon("check"),
      color = "aqua",  # Bleu
      width = 4
    )
  })
  
  output$nb3 <- renderValueBox({
    moy1 = df %>% summarise(round(mean(age_user, na.rm = TRUE)))
    valueBox(
      value = moy1,
      subtitle = h2("Age moyen des participants"),
      icon = icon("user"),
      color = "light-blue",  # Orange
      width = 4
    )
  })
  
  output$nb4 <- renderValueBox({
    moy2 = df %>% summarise(round(mean(age_entente_aut, na.rm = TRUE)))
    valueBox(
      value = moy2,
      subtitle = h2("Age moyen auquel les participants ont entendu parler de Cheick Anta Diop la première fois"),
      icon = icon("user"),
      color = "fuchsia",  # Rouge
      width = 12
    )
  })
  
  output$texte1 = renderText({
    paste("DESCRIPTION ")})
  output$texte = renderText({
    paste("Dans le cadre d’une série d’études sur des personnalités d’Afrique et de sa Diaspora, un sondage a été réalisé sur Cheikh Anta Diop. Les données collectées visent à mesurer la connaissance de Cheikh Anta Diop et de ses travaux.
Cette interface fait donc l'objet d'un projet qui explore les données relatives à l'enquête visant a évaluer le niveau de connaissance de Cheikh Anta Diop. Notre objectif est de montrer, clairement et directement, des informations pertinentes sur les résultats de l'enquête   en utilisant des points de vue peu explorés. Pour ce faire, nous abordons des sujets tels que la visualisation des données et les mesures sociodémographiques des personnes interrogées. 
 L'utilisateur peut comparer d'importantes variables sociodémographiques avec la proportion d'avis de chaque position ou perception et ainsi comprendre le profil des personnes interrogées.  En outre, plusieurs avis et informations sur les résultats peuvent être trouvés et explorés dans cette application. ")})
  
  # Tracer le graphique pour la Page 1
  output$plot1_connaissance_base <- renderPlot({
    req(credentials()$user_auth)
    donnees <- data
    pourcentages <- prop.table(table(donnees$connaiss)) * 100
    
    donnees <- donnees %>%
      group_by(connaiss) %>%
      summarize(count = n()) %>%
      mutate(percentage = count / sum(count) * 100)
    
    plot1 <- ggplot(donnees, aes(x = "", y = percentage, fill = connaiss)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      geom_text(
        aes(label = paste0(round(percentage, 1), "%")),
        position = position_stack(vjust = 0.5),
        size = 3.5,
        color = "black"
      ) +
      labs(
        title = "Repartion des personnes connaissant l'auteur",
        x = "Avez-vous déjà entendu parler de Cheikh Anta Diop?",
        y = "Pourcentage"
      ) +
      theme_minimal() +
      coord_polar("y", start = 0) +
      labs(fill = "Connaissance ?") + theme(
        plot.title = element_text(face = "bold", size = 14)
      )
    
    print(plot1)
    
    
  })
  output$plot2_connaissance_base <- renderPlot({
    req(credentials()$user_auth)
    donnees <- data
    pourcentages <- prop.table(table(donnees$connaiss)) * 100
    
    plot1 <- ggplot(data, aes(x = connaiss, fill = Pays_naiss_user)) +
      geom_bar(position = "dodge", color = "black", stat = "count") +
      labs(title = "Repartion des personnes connaissant l'auteur en fonction du pays de naissance ",
           x = "Pays naissance",
           y = "Count") +
      theme_minimal()+
      labs(fill = "Pays") + theme(
        plot.title = element_text(face = "bold", size = 14)
      )
    
    # Graphiques supplémentaires (remplacez-les par vos propres données et esthétiques)
    
    
    print(plot1)
    
  })
  
  output$plot3_connaissance_base <- renderPlot({
    req(credentials()$user_auth)
    ggplot(data, aes(x = connaiss, fill = age_user)) +
      geom_bar(position = "dodge", color = "black", stat = "count") +
      labs(title = "Representation de la connaissance en fonction de la tranche d'age",
           x = "connaissance",
           y = "Total") +
      theme_minimal()  +
      labs(fill = "Age")+theme(
        plot.title = element_text(face = "bold", size = 14)
      )
    
  })
  
  
  output$plot4_connaissance_base <- renderPlot({
    req(credentials()$user_auth)
    data <- data %>% mutate(education_user= ifelse(is.na(education_user), "aucune reponse",  education_user))
    donnees <- data %>%
      group_by(education_user) %>%
      summarize(count = n())
    donnees$fraction = donnees$count / sum(donnees$count)
    donnees$ymax = cumsum(donnees$fraction)
    donnees$ymin = c(0, head(donnees$ymax, n=-1))
    ggplot(donnees, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=education_user)) +
      geom_rect() + labs(title = "Repartition de connaissance de l'auteur par niveau éducation")+
      coord_polar(theta="y") + xlim(c(2, 4)) +theme(
        plot.title = element_text(face = "bold", size = 14)
      )
      
      

    
    
    


    #ggplot(donnees, aes(x = "", y = count, fill = education_user)) +
      #geom_rect() +
      #coord_polar(theta = "y") +
     # theme_void() + labs(title = "Connaissance de Cheich Anta Diop en fonction du niveau d'education")
    # Affichage du diagramme
   
    
  })
  
  
  output$plot1_connaissance_generale <- renderPlot({
    req(credentials()$user_auth)

    
    ggplot(df_sep1, aes(y = celebres_soutien_aut)) +
      geom_bar(fill = "skyblue", color = "black", stat = "count", show.legend = FALSE) +  
      theme_minimal() +
      
      # Ajouter des couleurs au texte des axes
      theme(axis.text = element_text(color = "black")) +
      
      labs(
        title = "Repartition des auteurs celebres ayant soutenu Cheick Anta Diop",
        y = "Auteurs celebres"  # Vous pouvez personnaliser le nom de l'axe Y
        # Ajoutez d'autres étiquettes ici selon vos besoins
      ) +
      
      theme(
        plot.title = element_text(face = "bold", size = 14)
      )
    
  })
  
  
  output$plot2_connaissance_generale <- renderPlot({
    req(credentials()$user_auth)
    dt <- data %>%
      mutate(oeuvres_auto_rcd = recode(oeuvres_aut,
                                       "afrique noire  precoloniale" = "A",
                                       "barbabrie ou civilisation" = "B",
                                       "l'unité culturelle de l'afrique noire" = "C",
                                       "mauvaise reponse" = "D",
                                       "nations nègres et culture" = "E",
                                       "nations nègres et culture , civilisations ou barbarie" = "F",
                                       "nations negres et culture, civilisation ou barbarie,l’afrique noire pré coloniale" = "G",
                                       "nation negre et culture,les fndements culturels et techniques d'un  federal Afrique noir" = "H",
                                       "non" = "I",
                                       "nouvelles recherches sur l'égyptien ancien et les langues négro-africaines modernes" = "J",
                                       "unité culturelle de l'afrique noire" = "K"))
    # Utilisez les données réelles de votre ensemble de données
    # Assurez-vous d'avoir recodé la variable oeuvres_auto en oeuvres_auto_rcd
    # my_data <- read.csv("chemin/vers/votre/fichier.csv")
    
    # Créez un graphique à barres
    ggplot(dt, aes(x = age_user, fill = oeuvres_auto_rcd)) +
      geom_bar(position = "stack", stat = "count") +
      labs(title = "Répartition de oeuvres connues de l'auteur par tranche d'âge",
           x = "Tranche d'âge",
           y = "Nombre d'occurrences",
           fill = "Oeuvres ") +
      theme_minimal()+theme(
        plot.title = element_text(face = "bold", size = 14)  
      ) 

    
  })
  
  
  output$plot3_connaissance_generale <- renderPlot({
    req(credentials()$user_auth)
    data <- data %>%
      mutate(oeuvres_auto_rcd = recode(oeuvres_aut,
                                       "afrique noire  precoloniale" = "A",
                                       "barbabrie ou civilisation" = "B",
                                       "l'unité culturelle de l'afrique noire" = "C",
                                       "mauvaise reponse" = "D",
                                       "nations nègres et culture" = "E",
                                       "nations nègres et culture , civilisations ou barbarie" = "F",
                                       "nations negres et culture, civilisation ou barbarie,l’afrique noire pré coloniale" = "G",
                                       "nation negre et culture,les fndements culturels et techniques d'un  federal Afrique noir" = "H",
                                       "non" = "I",
                                       "nouvelles recherches sur l'égyptien ancien et les langues négro-africaines modernes" = "J",
                                       "unité culturelle de l'afrique noire" = "K"))
    ggplot(data, aes(x = education_user, fill = oeuvres_auto_rcd)) +
      geom_bar(position = "stack", stat = "count") +
      labs(title = "Répartition des oeuvres connues par niveau d'étude",
           x = "Niveau d'education",
           y = "Nombre d'occurrences",
           fill = "Oeuvres ") +
      theme_minimal() + theme(
        plot.title = element_text(face = "bold", size = 14)  
      )    

    
  })
  
  
  output$plot4_connaissance_generale <- renderPlot({
    req(credentials()$user_auth)
    donnees <- data %>%
      group_by(Period_hist) %>%
      summarize(count = n())
    donnees$fraction = donnees$count / sum(donnees$count)
    donnees$ymax = cumsum(donnees$fraction)
    donnees$ymin = c(0, head(donnees$ymax, n=-1))
    ggplot(donnees, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Period_hist)) +
      geom_rect() +
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      labs(fill = "periode historique",title="Répartition de la période historique ") + theme(
      plot.title = element_text(face = "bold", size = 14)  
    )

    
  })
  
  

  
  output$plot1_connaissance_avancee <- renderPlot({
    req(credentials()$user_auth)
    ggplot(df_sep1, aes(y = celebres_soutien_aut)) +
      geom_bar(fill = "skyblue", color = "black", stat = "count", show.legend = FALSE) +  
      theme_minimal() +
      
      # Ajouter des couleurs au texte des axes
      theme(axis.text = element_text(color = "black")) +
      
      labs(
        title = "Repartition des auteurs celebres ayant soutenu Cheick Anta Diop",
        y = "Auteurs celebres"  # Vous pouvez personnaliser le nom de l'axe Y
        # Ajoutez d'autres étiquettes ici selon vos besoins
      ) +
      
      theme(
        plot.title = element_text(face = "bold", size = 14)
      )
    
    # Graphiques supplémentaires (remplacez-les par vos propres données et esthétiques)
    
    
    
    
  })
  
  output$plot2_connaissance_avancee <- renderPlot({
    req(credentials()$user_auth)
    
    text <- paste(data$oeuvres_lu, collapse = " ")
    wordcloud(words = strsplit(text, " ")[[1]], min.freq = 1, scale = c(3, 0.5),colors=brewer.pal(8, "Dark2")) 
    title(main = "Frequence d'apparition des œuvre lues")
    # Graphiques supplémentaires (remplacez-les par vos propres données et esthétiques)
    
    
    
    
  })
  

  
  

  
  
  output$plot1_influence_perception <- renderPlot({
    req(credentials()$user_auth)
    text <- paste(data$principales_critiques, collapse = " ")
    wordcloud(words = strsplit(text, " ")[[1]], min.freq = 1, scale = c(3, 0.5),colors=brewer.pal(7, "Dark2"))
    title(main = "Frequence d'apparition des principales_critiques ")

    
  })
  
  output$plot2_influence_perception <- renderPlot({
    req(credentials()$user_auth)
    df=table(data$perception_gen)
    df=data.frame(df)
    plt <- ggplot(df) +
      # Add bars to represent the cumulative track lengths
      geom_col(
        aes(
          x = factor(Var1, levels = Var1[order(Freq)]),
          y = Freq,
          fill = Freq
        ),
        show.legend = TRUE,
        alpha = 0.9
      ) +
      
      # Add dots to represent the mean gain
      geom_point(
        aes(
          x = factor(Var1, levels = Var1[order(Freq)]),
          y = Freq
        ),
        size = 3,
        color = "gray12"
      ) +
      
      # Lollipop shaft for mean gain per region
      geom_segment(
        aes(
          x = factor(Var1, levels = Var1[order(Freq)]),
          y = 0,
          xend = factor(Var1, levels = Var1[order(Freq)]),
          yend = Freq
        ),
        linetype = "dashed",
        color = "gray12"
      ) + 
      
      # Make it circular!
      coord_polar(theta = "x", start = 0) +
      
      # Ajouter un titre ici
      ggtitle("Répartition de la perception de l'auteur") +
      
      # Personnaliser le style du titre (en gras)
      theme(
        plot.title = element_text(face = "bold", size = 14)
      )   + labs(
        x = "perception générale de l'auteur",  # Nom de l'axe des x
        y = "Fréquence"  # Nom de l'axe des y
      )
    
    plt
    
  })
  
  output$plot3_influence_perception <- renderPlot({
    req(credentials()$user_auth)
    table(data$pertinence_idees) # Diagramme a barre.
    pp <- data %>%
      filter(!is.na(pertinence_idees)) %>%
      ggplot(aes(x = pertinence_idees, fill = pertinence_idees)) +
      geom_bar( alpha = 0.9, position = "dodge") +
      ggtitle("Fréquence des personnes approuvants la pertinence des idées ") +
      theme_ipsum() +
      theme(
        plot.title = element_text(size = 15)
      )
    
    print(pp)

    
  })
  
  output$plot4_influence_perception <- renderPlot({
  table(data$pertinence_idees) # Diagramme a barre.
  pp <- data %>%
    filter(!is.na(pertinence_idees)) %>%
    ggplot(aes(x = pertinence_idees, fill = age_user)) +
    geom_bar(color = "#e9ecef", alpha = 0.9, position = "dodge") +
    ggtitle("Personnes approuvants la pertinence des idées par age") +
    theme_ipsum() +
    theme(
      plot.title = element_text(size = 15)
    )
  
  print(pp)
  
})

  # Tracer le graphique pour la Page 2

  
  # Ajouter plus de graphiques et d'onglets si nécessaire
}

shinyApp(ui = ui, server = server)