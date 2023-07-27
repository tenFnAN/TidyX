
## TidyX Episode 76: Joins with updating Databases - Shiny

### Packages ---------------------------------------------
library(tidyverse)
library(rvest)
library(janitor)

library(RSQLite)
library(DBI)

library(shiny)
library(shinyWidgets)
library(DT)


db_path <- "TidyTuesday_Explained/076-Databases_with_Shiny_4/nba_playoffs.db"
 
# SHINY APP for coaches to leave comments ----

db_con <- dbConnect(
  drv = RSQLite::SQLite(),
  here::here(db_path)
)

games_available <- tbl(db_con, "game_ids") %>% 
  pull(game_info)


##disconnect
dbDisconnect(db_con)

### Shiny app with an auto refresh
ui <- fluidPage(
  
  title = "2021 Playoffs",
  
  sidebarPanel(
    
    pickerInput(
      inputId = "game_selection",
      label = "Select Game:",
      choices = games_available
    ),
  ),
  
  mainPanel(
    DTOutput(outputId = "pbp_table"),
    uiOutput(outputId = "commit_button_display")
  )
  
)


server <- function(input, output, session){
  
  displayedData <- reactiveValues()
  
  observeEvent(input$game_selection,{
    
    req(input$game_selection)
    
    ## connect to database
    db_con <- dbConnect(
      drv = RSQLite::SQLite(),
      here::here(db_path)
    )
    
    ##disconnect when reactive finishes
    on.exit(dbDisconnect(db_con))
    
    game_id <- tbl(db_con, "game_ids") %>% 
      filter( game_info == !!input$game_selection) %>% 
      pull(game_id)
    
    comments <- tbl(db_con, paste0("game_comments_",game_id))
    
    pbp <- tbl(db_con, paste0("game_pbp_",game_id)) %>% 
      left_join(comments, by = "play_id_num") %>% 
      arrange(play_id_num)
    
    displayedData$game_id <- game_id
    displayedData$comments <- comments %>% collect() 
    displayedData$pbp <- pbp %>% collect() 
    
    displayedData$comment_commit_up_to_date <- TRUE
    
  })
  
  ## Data rendering 
  output$pbp_table <- renderDT({
    displayedData$pbp
  },
  selection = 'none',
  rownames = FALSE,
  editable = TRUE,
  escape = FALSE)
  
  ## when updated, 
  proxy = dataTableProxy('pbp_table')
  observeEvent(input$pbp_table_cell_edit,{
    
    info = input$pbp_table_cell_edit
    i = info$row
    j = info$col + 1  # column index offset by 1
    v = info$value
    
    ## only comment column can be edited
    if(colnames(displayedData$pbp)[j] == "comment"){
      
      play_num <- displayedData$pbp$play_id_num[i]
      
      displayedData$comments <- bind_rows(
        displayedData$comments[!displayedData$comments$play_id_num == play_num,],
        data.frame(
          play_id_num = play_num, 
          comment = v
        )
      )
      
      displayedData$pbp <- displayedData$pbp %>% 
        select(-comment) %>% 
        left_join(displayedData$comments, by = "play_id_num") %>% 
        arrange(play_id_num)
      
      displayedData$comment_commit_up_to_date <- FALSE
      
    }
    
    replaceData(proxy, displayedData$pbp, resetPaging = FALSE, rownames = FALSE)
    
  })
  
  output$commit_button_display <- renderUI({
    if(!displayedData$comment_commit_up_to_date){
      actionButton("commit","click to save comments")
    }
  })
  
  ## commit updates and share comments
  observeEvent(input$commit,{
    # browser()
    db_con <- dbConnect(
      drv = RSQLite::SQLite(),
      here::here(db_path)
    )
    
    ##disconnect when reactive finishes
    on.exit(dbDisconnect(db_con))
    
    dbWriteTable(
      db_con,
      name = paste0("game_comments_", displayedData$game_id),
      as.data.frame(displayedData$comments),
      overwrite = TRUE
    )
    
    dbWriteTable(
      db_con,
      name = paste0("game_comments_update_time_", displayedData$game_id),
      data.frame(time = Sys.time()),
      overwrite = TRUE
    )
    
    displayedData$comment_commit_up_to_date <- TRUE
    
    
  })
  
  ## Add polling of comments table to check if there are committed updated
  comments_updated <- reactivePoll(
    
    intervalMillis = 5000,
    
    session,
    
    checkFunc = function(){
      
      ## connect to database
      db_con <- dbConnect(
        drv = RSQLite::SQLite(),
        here::here(db_path)
      )
      
      ##disconnect when reactive finishes
      on.exit(dbDisconnect(db_con))
      
      if(!is.null(displayedData$game_id)){
        dbGetQuery(db_con, paste0("SELECT time FROM game_comments_update_time_", displayedData$game_id))
      }
      
    },
    
    valueFunc = function() {
      
      ## connect to database
      db_con <- dbConnect(
        drv = RSQLite::SQLite(),
        here::here(db_path)
      )
      
      ##disconnect when reactive finishes
      on.exit(dbDisconnect(db_con))
      
      ## query database
      tbl(db_con, paste0("game_comments_",displayedData$game_id)) %>% collect()
    }
  ) 
  observeEvent(comments_updated(),{
    
    ## check if any differences between committed comments and current comments
    diff_comments <- anti_join(
      comments_updated(),
      displayedData$comments, 
      by= colnames(displayedData$comments)
    )
    
    if(nrow(diff_comments) > 0){
      
      new_comments <- anti_join(
        comments_updated(),
        displayedData$comments, 
        by= "play_id_num"
      )
      
      conflicting_comments <- inner_join(
        diff_comments,
        displayedData$comments, 
        by= "play_id_num"
      )
      
      new_comments_2 <- do.call('rbind',lapply(seq_len(nrow(conflicting_comments)),function(i){
        db_comment <- conflicting_comments[i,"comment.x"]
        local_comment <- conflicting_comments[i,"comment.y"]
        
        data.frame(
          play_id_num = conflicting_comments[i,"play_id_num"],
          comment = paste0(db_comment,"<br>",local_comment)
        )
      }))
      
      updated_comments <- bind_rows(
        new_comments, 
        new_comments_2
      )
      
      displayedData$comments <- bind_rows(
        anti_join(
          displayedData$comments,
          updated_comments,
          by = "play_id_num"
        ),
        updated_comments
      )
      
      displayedData$pbp <- displayedData$pbp %>% 
        select(-comment) %>% 
        left_join(displayedData$comments, by = "play_id_num") %>% 
        arrange(play_id_num)
      
      displayedData$comment_commit_up_to_date <- FALSE
      
      replaceData(proxy, displayedData$pbp, resetPaging = FALSE, rownames = FALSE)
      
    }
    
  }) 
}

# kiedy aktualizacja
if(F){
  
  as.POSIXct(dbGetQuery(db_con, paste0("SELECT time FROM game_comments_update_time_", 401327715))$time, origin = '1970-01-01')
  
  tbl(db_con, paste0("game_comments_",401327715)) %>% collect()
}
shinyApp(ui, server)




