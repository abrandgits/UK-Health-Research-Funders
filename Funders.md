# Using NIHR Open Data Platform to Identify the Top 10 Funders in all of the NIHR Specialities

To update this report please download data manually by following the instructions below.

### Instructions to Download Data

1. Go to  http://public-odp.nihr.ac.uk/qlikview/

2. Click on the Find A Clinical Research Study.
3. Click on the Search for a Study button.
4. Control Click Lead Administration ENG and WAL.
5. Right Click on Table Header.
6. Click on the Send to Excel option on the dropdown menu.

***


```R
d<-format(as.POSIXct(file.info("Data.xlsx")$mtime ),"%H:%M:%S on %d/%m/%Y")

cat(paste0("Data was downloaded from NIHR CRN Public Search at: ",d))
```

    Data was downloaded from NIHR CRN Public Search at: 15:55:20 on 25/02/2021


```R
library("XLConnect")
library("tools")
library("stringr")
library("kableExtra")
library("IRdisplay")
```

    Loading required package: XLConnectJars
    
    XLConnect 0.2-15 by Mirai Solutions GmbH [aut],
      Martin Studer [cre],
      The Apache Software Foundation [ctb, cph] (Apache POI),
      Graph Builder [ctb, cph] (Curvesapi Java library)
    
    http://www.mirai-solutions.com
    https://github.com/miraisolutions/xlconnect
    
    Warning message:
    "package 'stringr' was built under R version 3.5.3"
    


```R
# Load Downloaded Data

wb <- loadWorkbook("Data.xlsx")
df <- readWorksheet(wb, sheet=1)
```


```R
# Extract Data

spec<-unique(df$Managing.Specialty)

for (i in 1:length(spec)){
  
  df1<-subset(df,df$Managing.Specialty==spec[i])
  
  # Remove Trials with Multiple Funders
  df1<-df1[grep(";",df1$Funder.s.,value=FALSE,invert = TRUE),]
  
  # Remove Department of Health
  df1<-df1[grep("Department of Health",df1$Funder.s.,value=FALSE,invert = TRUE),]
  
  # Combine all NIHR funders
  df1$Funder.s.<-str_replace(df1$Funder.s.,".*NIHR.*","NIHR")
  
  
  odf<-as.data.frame(table(df1$Funder.s.))
  colnames(odf)<-c("Funder","Freq")
  
  odf<-odf[order(odf$Freq,decreasing = TRUE),]
  
  # Remove Funder Not Given
  odf<-odf[odf$Funder!="-",]
  
  # Take the Top N Funders
  N<-10  
    
  odf<-odf[1:N,]
  
  # Add Percentage
    
  odf$Percent<-round((odf$Freq/sum(odf$Freq))*100,0)
  

  # Tidy up case of funders name
    
  odf$Funder<-as.character(odf$Funder)
  odf$Funder<-toTitleCase(odf$Funder)
    
  for (j in 1:nrow(odf)){
    
    caps<-str_count(odf$Funder[j], "[A-Z]")
    pro<-caps/nchar(odf$Funder[j])
    
    
    if (pro>0.50){
      odf$Funder[j]<-toTitleCase(tolower(odf$Funder[j]))
    }
    
    odf$Funder[j]<-str_replace(odf$Funder[j],"Nihr","NIHR")
    odf$Funder[j]<-str_replace(odf$Funder[j],"Uk","UK")

    
  }
    
  colnames(odf)[1]<-paste0(str_replace_all(spec[i],",","")) 
    
    
  odf %>%  
  kable(format = "html", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = "striped", full_width = FALSE, position = "left") %>%
  as.character() %>%
  display_html()
      
   
}

```


<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Infection </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 118 </td>
   <td style="text-align:right;"> 29 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:right;"> 17 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wellcome Trust </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 15 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Commission </td>
   <td style="text-align:right;"> 48 </td>
   <td style="text-align:right;"> 12 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> British HIV Association (BHIVA) </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Gilead Sciences Inc </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Viiv Healthcare Limited </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Innovate UK </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Department of Health and Social Care </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Merck Sharp &amp; Dohme Limited </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
</tbody>
</table>



<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Cancer </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Cancer Research UK </td>
   <td style="text-align:right;"> 580 </td>
   <td style="text-align:right;"> 42 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 322 </td>
   <td style="text-align:right;"> 24 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Organisation for Research and Treatment of Cancer </td>
   <td style="text-align:right;"> 116 </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 94 </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Bloodwise </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Commission </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AstraZeneca UK Limited </td>
   <td style="text-align:right;"> 38 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Macmillan Cancer Support </td>
   <td style="text-align:right;"> 38 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Tenovus Cancer Care </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Yorkshire Cancer Research </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
</tbody>
</table>



<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Critical Care </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 86 </td>
   <td style="text-align:right;"> 52 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Society of Intensive Care Medecine </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wellcome Trust </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Commission </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Resuscitation Council (UK) </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ministry of Defence </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> The Intensive Care Society </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Engineering and Physical Sciences Research Council (EPSRC) </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Fresenius Medical Care </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
</tbody>
</table>



<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Health Services Research </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 275 </td>
   <td style="text-align:right;"> 63 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Health Foundation </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Department of Health and Social Care </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Commission </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Engineering and Physical Sciences Research Council (EPSRC) </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wellcome Trust </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Economic and Social Research Council (ESRC) </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Health and Care Research Wales </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Innovate UK </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
</tbody>
</table>



<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Stroke </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 133 </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Stroke Association </td>
   <td style="text-align:right;"> 95 </td>
   <td style="text-align:right;"> 29 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 34 </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wellcome Trust </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> British Heart Foundation (BHF) </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> The Dunhill Medical Trust </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Commission </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Welsh Government </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Health Foundation </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Engineering and Physical Sciences Research Council (EPSRC) </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
</tbody>
</table>



<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Mental Health </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 805 </td>
   <td style="text-align:right;"> 57 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 217 </td>
   <td style="text-align:right;"> 15 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wellcome Trust </td>
   <td style="text-align:right;"> 99 </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Department of Health and Social Care </td>
   <td style="text-align:right;"> 81 </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Commission </td>
   <td style="text-align:right;"> 76 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Economic and Social Research Council (ESRC) </td>
   <td style="text-align:right;"> 68 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Health Foundation </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Health and Care Research Wales </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Autistica </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Healthcare Quality Improvement Partnership </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
</tbody>
</table>



<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Primary Care </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 706 </td>
   <td style="text-align:right;"> 68 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 71 </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Department of Health and Social Care </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Versus Arthritis </td>
   <td style="text-align:right;"> 53 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Commission </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cancer Research UK </td>
   <td style="text-align:right;"> 38 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Economic and Social Research Council (ESRC) </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wellcome Trust </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Health Foundation </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> The Dunhill Medical Trust </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
</tbody>
</table>



<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Ageing </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 99 </td>
   <td style="text-align:right;"> 56 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Commission </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Economic and Social Research Council (ESRC) </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> The Dunhill Medical Trust </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> British Geriatrics Society </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Innovate UK </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Health and Care Research Wales </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> The Abbeyfield Research Foundation </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Department of Health and Social Care </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
</tbody>
</table>



<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Metabolic and Endocrine Disorders </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 22 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 21 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wellcome Trust </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 19 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Biotechnology &amp; Biological Sciences Research Council </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 11 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Commission </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> British Heart Foundation (BHF) </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Diabetes UK </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pfizer Inc. </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Food Standards Agency </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Genzyme Corporation </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
</tbody>
</table>



<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Musculoskeletal Disorders </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Versus Arthritis </td>
   <td style="text-align:right;"> 216 </td>
   <td style="text-align:right;"> 36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 198 </td>
   <td style="text-align:right;"> 33 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:right;"> 11 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Engineering and Physical Sciences Research Council (EPSRC) </td>
   <td style="text-align:right;"> 23 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wellcome Trust </td>
   <td style="text-align:right;"> 23 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Commission </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pfizer Inc. </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> The Chartered Society of Physiotherapy Charitable Trust </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Orthopaedic Research UK </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Innovate UK </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
</tbody>
</table>



<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Oral and Dental Health </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 48 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Oral and Dental Research Trust </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 15 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Royal College of Surgeons of England </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> The British Orthodontic Society </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> The Dunhill Medical Trust </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Versus Arthritis </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Biotechnology &amp; Biological Sciences Research Council </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Society of Endodontology (Ese) </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Health and Care Research Wales </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
</tbody>
</table>



<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Dementias and Neurodegeneration </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 176 </td>
   <td style="text-align:right;"> 31 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 85 </td>
   <td style="text-align:right;"> 15 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Parkinson's UK </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Alzheimer's Society </td>
   <td style="text-align:right;"> 55 </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Commission </td>
   <td style="text-align:right;"> 48 </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Alzheimer's Research UK </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Economic and Social Research Council (ESRC) </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Motor Neurone Disease Association (MNDA) </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CHDI Management, Inc </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wellcome Trust </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
</tbody>
</table>



<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Children </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 305 </td>
   <td style="text-align:right;"> 52 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Commission </td>
   <td style="text-align:right;"> 76 </td>
   <td style="text-align:right;"> 13 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 61 </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Action Medical Research </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wellcome Trust </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Versus Arthritis </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> National Institutes of Health (NIH), United States </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SPARKS (Sport Aiding Medical Research for Kids) </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> British Heart Foundation (BHF) </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Department of Health and Social Care </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
</tbody>
</table>



<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Renal Disorders </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 91 </td>
   <td style="text-align:right;"> 34 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Kidney Research UK (KRUK) </td>
   <td style="text-align:right;"> 59 </td>
   <td style="text-align:right;"> 22 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> British Renal Society (BRS) </td>
   <td style="text-align:right;"> 43 </td>
   <td style="text-align:right;"> 16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> British Heart Foundation (BHF) </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wellcome Trust </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Commission </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Baxter Healthcare Corporation </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Prostate Cancer UK </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Versus Arthritis </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
</tbody>
</table>



<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Surgery </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 96 </td>
   <td style="text-align:right;"> 63 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Commission </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Bowel Disease Research Foundation (BDRF) </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Intuitive Surgical, Inc. </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Stryker Corporation </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> British Heart Foundation (BHF) </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> National Institute of Academic Anaesthesia </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Royal College of Surgeons of England </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wellcome Trust </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
</tbody>
</table>



<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Neurological Disorders </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 75 </td>
   <td style="text-align:right;"> 23 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Multiple Sclerosis Society (of Great Britain &amp; Northern Ireland) </td>
   <td style="text-align:right;"> 67 </td>
   <td style="text-align:right;"> 21 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:right;"> 17 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wellcome Trust </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 15 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Commission </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Epilepsy Research UK </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Department of Health and Social Care </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Engineering and Physical Sciences Research Council (EPSRC) </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Epilepsy Action </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Muscular Dystrophy UK </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
</tbody>
</table>



<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Trauma and Emergency Care </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 146 </td>
   <td style="text-align:right;"> 67 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Royal College of Emergency Medicine </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> College of Paramedics </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AO Foundation </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Commission </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Health Foundation </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> The Scar Free Foundation </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Health and Care Research Wales </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Department of Health and Social Care </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
</tbody>
</table>



<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Public Health </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 83 </td>
   <td style="text-align:right;"> 54 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Department of Health and Social Care </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wellcome Trust </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cancer Research UK </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Commission </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Health and Care Research Wales </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Public Health England </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Economic and Social Research Council (ESRC) </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Health Foundation </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
</tbody>
</table>



<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Cardiovascular Disease </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> British Heart Foundation (BHF) </td>
   <td style="text-align:right;"> 310 </td>
   <td style="text-align:right;"> 47 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 136 </td>
   <td style="text-align:right;"> 21 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 51 </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Commission </td>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medtronic, Inc. </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Boston Scientific </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wellcome Trust </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Heart Research UK </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Engineering and Physical Sciences Research Council (EPSRC) </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AstraZeneca UK Limited </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
</tbody>
</table>



<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Dermatology </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 52 </td>
   <td style="text-align:right;"> 31 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> British Skin Foundation </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 17 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Psoriasis Association </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wellcome Trust </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Debra </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Commission </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Biotechnology &amp; Biological Sciences Research Council </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pfizer Inc. </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Raynaud's and Scleroderma Association </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
</tbody>
</table>



<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Respiratory Disorders </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 143 </td>
   <td style="text-align:right;"> 35 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 95 </td>
   <td style="text-align:right;"> 23 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Asthma UK </td>
   <td style="text-align:right;"> 34 </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wellcome Trust </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Asthma UK and British Lung Foundation Partnership </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Commission </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AstraZeneca UK Limited </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cystic Fibrosis Trust </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Glaxosmithkline Research &amp; Development Limited </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> j p Moulton Charitable Foundation </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
</tbody>
</table>



<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Haematology </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 26 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Bayer Healthcare AG </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 13 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 13 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pfizer Inc. </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 13 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wellcome Trust </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> British Heart Foundation (BHF) </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Commission </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Immune Thrombocytopenic Purpura (ITP) Support Association </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Nhs Blood and Transplant </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Novo Nordisk a/S </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
</tbody>
</table>



<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Diabetes </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 120 </td>
   <td style="text-align:right;"> 29 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Diabetes UK </td>
   <td style="text-align:right;"> 108 </td>
   <td style="text-align:right;"> 26 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Commission </td>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:right;"> 9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wellcome Trust </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Juvenile Diabetes Research Foundation Limited (JDRF) </td>
   <td style="text-align:right;"> 23 </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> British Heart Foundation (BHF) </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Novo Nordisk a/S </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Diabetes Research &amp; Wellness Foundation (DRWF) </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Novo Nordisk UK Research Foundation </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
</tbody>
</table>



<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Genetics </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Wellcome Trust </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 26 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 22 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 21 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Commission </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Department of Health and Social Care </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Fight for Sight (British Eye Research Foundation) </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> British Heart Foundation (BHF) </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cancer Research UK </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Economic and Social Research Council (ESRC) </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Action Medical Research </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
</tbody>
</table>



<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Reproductive Health and Childbirth </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 217 </td>
   <td style="text-align:right;"> 51 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Tommy's the Baby Charity </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wellcome Trust </td>
   <td style="text-align:right;"> 34 </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wellbeing of Women </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Commission </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Economic and Social Research Council (ESRC) </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Action Medical Research </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> British Maternal and Fetal Medicine Society </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Department of Health and Social Care </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
</tbody>
</table>



<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Gastroenterology </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 62 </td>
   <td style="text-align:right;"> 33 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Crohn's and Colitis UK </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 14 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 14 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Biotechnology &amp; Biological Sciences Research Council </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 11 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wellcome Trust </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cancer Research UK </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Guts UK Charity </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Takeda Pharmaceutical Company Limited </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Bowel and Cancer Research </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Bowel Disease Research Foundation (BDRF) </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
</tbody>
</table>



<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Ear Nose and Throat </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 38 </td>
   <td style="text-align:right;"> 37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 25 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wellcome Trust </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Deafness Research UK </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Commission </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> National Institutes of Health (NIH), United States </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Royal National Institute for Deaf People (Action on Hearing Loss) </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Advanced Bionics UK Limited </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> British Tinnitus Association </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cochlear (UK) Limited </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
</tbody>
</table>



<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Ophthalmology </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 71 </td>
   <td style="text-align:right;"> 33 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Fight for Sight (British Eye Research Foundation) </td>
   <td style="text-align:right;"> 58 </td>
   <td style="text-align:right;"> 27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wellcome Trust </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> International Glaucoma Association Limited </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> The College of Optometrists </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Bayer Healthcare AG </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rayner Intraocular Lenses Limited </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Allergan Limited </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Economic and Social Research Council (ESRC) </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
</tbody>
</table>



<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Anaesthesia Perioperative Medicine and Pain Management </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> National Institute of Academic Anaesthesia </td>
   <td style="text-align:right;"> 48 </td>
   <td style="text-align:right;"> 37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 31 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Obstetric Anaesthetists' Association (OAA) </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Commission </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medtronic, Inc. </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pain Relief Foundation </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Society of Anaesthesiology </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> National Health and Medical Research Council of Australia </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Boston Scientific </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
</tbody>
</table>



<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Hepatology </th>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> NIHR </td>
   <td style="text-align:right;"> 38 </td>
   <td style="text-align:right;"> 35 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Commission </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 18 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Medical Research Council </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wellcome Trust </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> European Association for the Study of the Liver </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Livernorth </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Department of Health and Social Care </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Intercept Pharmaceuticals Inc </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cancer Research UK </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Innovate UK </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
</tbody>
</table>


***


```R
cat(paste("Created on:",format(Sys.time(), "%d/%m/%Y %X")))
```

    Created on: 08/04/2021 18:36:55


```R
print(sessionInfo())
```

    R version 3.5.2 (2018-12-20)
    Platform: x86_64-w64-mingw32/x64 (64-bit)
    Running under: Windows 7 x64 (build 7601) Service Pack 1
    
    Matrix products: default
    
    locale:
    [1] LC_COLLATE=English_United Kingdom.1252 
    [2] LC_CTYPE=English_United Kingdom.1252   
    [3] LC_MONETARY=English_United Kingdom.1252
    [4] LC_NUMERIC=C                           
    [5] LC_TIME=English_United Kingdom.1252    
    
    attached base packages:
    [1] tools     stats     graphics  grDevices utils     datasets  methods  
    [8] base     
    
    other attached packages:
    [1] IRdisplay_1.0        kableExtra_1.3.4     stringr_1.4.0       
    [4] XLConnect_0.2-15     XLConnectJars_0.2-15
    
    loaded via a namespace (and not attached):
     [1] Rcpp_1.0.4        highr_0.8         pillar_1.5.1      compiler_3.5.2   
     [5] base64enc_0.1-3   getPass_0.2-2     digest_0.6.18     uuid_0.1-4       
     [9] viridisLite_0.3.0 jsonlite_1.7.2    evaluate_0.14     lifecycle_0.2.0  
    [13] rlang_0.4.5       rstudioapi_0.9.0  IRkernel_1.1.1    xfun_0.22        
    [17] rJava_0.9-10      repr_1.1.3        httr_1.4.0        knitr_1.31       
    [21] xml2_1.3.2        systemfonts_1.0.1 webshot_0.5.2     svglite_2.0.0    
    [25] glue_1.4.2        R6_2.4.0          fansi_0.4.0       rmarkdown_2.1    
    [29] pbdZMQ_0.3-5      magrittr_1.5      scales_1.0.0      ellipsis_0.3.0   
    [33] htmltools_0.5.1.1 rvest_0.3.2       colorspace_1.4-1  utf8_1.1.4       
    [37] stringi_1.2.4     munsell_0.5.0     crayon_1.3.4     
    
