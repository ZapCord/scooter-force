## libraries
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(reshape2)
library(DT)
## functions
## vectorize assign, get and exists for convenience
assign_hash <- Vectorize(assign, vectorize.args = c("x", "value"))
get_hash <- Vectorize(get, vectorize.args = "x")
exists_hash <- Vectorize(exists, vectorize.args = "x")

## server
server<-function(input,output,session){
  observeEvent(input$refresh,{shinyjs::js$refresh()})
  hash = new.env(hash = TRUE, parent = emptyenv())
  observe({
    output$fileinputpanel<-renderPrint({ input$existing_data_input})
    if(is.null(input$existing_data_input) != TRUE){
      
      
      exist_data <- reactiveFileReader(1000,session,
                            filePath=toString(input$existing_data_input[4]),
                            readFunc=read.csv)
      d1_matrix<-exist_data()
      d1_matrix<-d1_matrix[!names(d1_matrix) %in% c("X")]
      
      updateSliderInput(session,"slider_time",max=nrow(d1_matrix))
      data<-matrix(nrow=10,ncol=16)
      for(j in 1:nrow(d1_matrix)){
        for(i in 1: ncol(d1_matrix)){
          if(i==1){
            time<-d1_matrix[j,i]
            row_num<-1
            col_num<-1
            next
          }else{
            data[row_num,col_num]<-d1_matrix[j,i]
            if(row_num < 10){
              row_num=row_num+1
            }else{
              row_num=1
              col_num=col_num+1
            }

          }

        }
        hash[[paste(time)]]<-as.data.frame(data)
      }
      
      keys<-reactiveVal()
      keys<-as.character(c(sort(as.numeric(ls(hash)))))
      
      data_m<-unlist(get_hash(keys[input$slider_time],hash))
      data_df<-matrix(nrow=10,ncol=16)
      row_num<-1
      col_num<-1
      for(i in 1:length(data_m)){
        data_df[row_num,col_num]<-data_m[i]
        if(row_num < 10){
          row_num=row_num+1
        }else{
          row_num=1
          col_num=col_num+1
        }
      }
      colnames(data_df)<-c("C1","C2","C3","C4","C5","C6","C7","C8",
                           "C9","C10","C11","C12","C13","C14","C15","C16")
      rownames(data_df)<-c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10")
      longData<-melt(data_df)
      names(longData)<-c("Rows","Columns","Force")
      output$table<-DT::renderDT(longData)
      g<-ggplot(longData, aes(x = Columns, y = Rows)) + 
        geom_raster(aes(fill=Force)) +
        scale_fill_gradient(low="grey90", high="red") +
        labs(x="Columns", y="Rows", title=
               paste("Scooter Force:",input$test,",",
                     input$subject,",","Time:",keys[input$slider_time],sep=" ")) +
        theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                           axis.text.y=element_text(size=9),
                           plot.title=element_text(size=11))
      output$plot<-renderPlot({g})
      
      subject<-reactive({
        subject<-gsub(" ","_",input$subject)
      })
      
      test_name_sub<-reactive({
        test_name_sub<-gsub(" ","_",input$test)
      })
      
      name<-reactive({
        name<-paste(test_name_sub(),"-",subject(),sep="")
      })
      
      jpeg_name<-reactive({
        jpeg_name<-paste(name(),".jpeg",sep="")
      })
      
      csv_name<-reactive({
        csv_name<-paste(name(),".csv",sep="")
      })
      
      output$savetable<-downloadHandler(
        filename=function(){
          paste(csv_name())
        },
        content=function(file){
          write.csv(longData,file)
        }
      )
      
      output$saveplot<-downloadHandler(
        filename=function(){paste(test_name_sub(),"-",subject(),".jpeg",sep="")},
        content=function(file){
          ggsave(file,plot=g)
        }
      )
      
      data_avg<-matrix(0,nrow=10,ncol=16)
      for(key in keys){
        data_m<-unlist(get_hash(key,hash))
        data_df<-matrix(nrow=10,ncol=16)
        row_num<-1
        col_num<-1
        for(i in 1:length(data_m)){
          data_avg[row_num,col_num]<-data_m[i]+data_avg[row_num,col_num]
          if(row_num < 10){
            row_num=row_num+1
          }else{
            row_num=1
            col_num=col_num+1
          }
        }
      }
      data_avg<-data_avg/(length(keys))
      colnames(data_avg)<-c("C1","C2","C3","C4","C5","C6","C7","C8",
                            "C9","C10","C11","C12","C13","C14","C15","C16")
      rownames(data_avg)<-c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10")
      
      longAvg<-melt(data_avg)
      names(longAvg)<-c("Rows","Columns","Force")
      output$avgtable<-DT::renderDT(longAvg)
      
      p<-ggplot(longAvg, aes(x = Columns, y = Rows)) + 
        geom_raster(aes(fill=Force)) +
        scale_fill_gradient(low="grey90", high="red") +
        labs(x="Columns", y="Rows", title=paste("Scooter Avg Force:",
                                                input$test,",",
                                                input$subject,sep=" ")) +
        theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                           axis.text.y=element_text(size=9),
                           plot.title=element_text(size=11))
      
      output$avgplot<-renderPlot(p)
      output$saveaveragetable<-downloadHandler(
        filename=function(){
          paste(name(),"-avg",".csv",sep="")
        },
        content=function(file){
          write.csv(longAvg,file)
        }
      )
      
      output$saveaverageplot<-downloadHandler(
        filename=function(){paste(test_name_sub(),"-",subject(),"-avg",".jpeg",sep="")},
        content=function(file){
          ggsave(file,plot=p)
        }
      )
    }
    
    #interp<-reactiveVal()
    #interp<-str_detect(input$checkGroup,"1")
    #print(interp)
    
    
    
  })
  
  
}