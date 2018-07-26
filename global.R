
# Load prison dropdown from s3
dt.prisons<-data.table::as.data.table(s3tools::s3_path_to_full_df("alpha-app-anvil-access-tool/prisons_and_offices_v2.csv",header=FALSE))


fields<-c("first_name","surname","prison","role","quantum_id","apps_needed","bentham","safety","categorisation")
foundErrors<-0
quantumErr<-0

responses<-s3tools::s3_path_to_full_df("alpha-app-anvil-access-tool/anvil-app-responses.csv",header=TRUE)
names(responses)<-fields[-6]

saveData<-function(data){
  
  #Reload data from S3
  responses<-data.table::as.data.table(s3tools::s3_path_to_full_df("alpha-app-anvil-access-tool/anvil-app-responses.csv",header=TRUE))
  
  data<-as.data.frame(t(data),stringsAsFactors=FALSE)
  
  if(exists("responses")){
    bentham_check<-as.integer("Bentham"%in%unlist(data[6]))
    safety_check<-as.integer("Safety Diagnostic Tool"%in%unlist(data[6]))
    cat_check<-as.integer("Prisoner Categorisation"%in%unlist(data[6]))
    data$bentham<-bentham_check
    data$safety<-safety_check
    data$categorisation<-cat_check
    data<-data[,-6]
    responses<-rbind(responses,data)
  }else{
    responses<-data
    responses$bentham<-as.integer("Bentham"%in%unlist(responses[1,6]))
    responses$safety<-as.integer("Safety Diagnostic Tool"%in%unlist(responses[1,6]))
    responses$categorisation<-as.integer("Prisoner Categorisation"%in%unlist(responses[1,6]))
  }
  
  #Format data and save to s3
  responses$prison<-as.character(responses$prison)
  responses$role<-as.character(responses$role)
  responses$first_name<-as.character(responses$first_name)
  responses$surname<-as.character(responses$surname)
  responses$quantum_id<-as.character(responses$quantum_id)
  responses$quantum_id<-tolower(responses$quantum_id)
  
  s3tools::write_df_to_csv_in_s3(responses,"alpha-app-anvil-access-tool/anvil-app-responses.csv",overwrite=TRUE,row.names=FALSE)
  
}
