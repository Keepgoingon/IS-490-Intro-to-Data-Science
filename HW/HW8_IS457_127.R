# Do not remove any of the comments. These are marked by #

# HW 8 

# (1). Please upload R code and report to Moodle 
#      with filename: HW8_IS457_YourClassID.


## Important: Make sure there is no identifying information on your printout, including name, username etc. 
## Only include your class ID on there.

### ClassID:127

### For this assignment, you will extract useful information from HTML and use Google Earth for data visualization. 
### The LatLon.rda file containing the country geographic coordinate is uploaded to Moodle.
### Look at detail instructions for the assignment in hw8_Intro.pdf.


### Part I. Create the data frame from HTML file (20 pts)

### Q1. Load LatLon.rda,
### install and load XML and RCurl libraries.

### Your code here
install.packages('RCurl')
install.packages('XML')
library(RCurl)
library(XML)
load("LatLon.rda")

### Q2. Download the html file from the url below, and parse it to html_text. (2 pts)
### Open the website and read it before coding.
### We will be working on the data from one of the tables: United Nations 2016-2017 global population data

url = "https://en.m.wikipedia.org/wiki/List_of_countries_by_population_(United_Nations)"

### Your code here
testfile='hw8.html'
download.file(url, testfile, mode="wb") # get data into test
html_text<- htmlParse(testfile)


### Q3. Read the tables in html_text with the readHTMLTable() function, set the 1st row as header.
### Now you should have a list object of 3 data frames, of which one is the table of population data.
### Coerce the table of population data to a data frame named "population". (2 pts)

### Your code here
table<-readHTMLTable(html_text,header = T)
population=data.frame(table[[3]])
class(population)
head(population,n=10)

### Q4. Let's simplify the data frame.
### Remove all other columns except country name, 2016 population, and 2017 population.
### Rename the 3 columns "Country", "Population_2016" and "Population_2017". (2 pts)

### Your code here
population=population[,c(2,5,6)]
colnames(population)=c("Country", "Population_2016" ,"Population_2017")
head(population)

### Q5. Recall our regular expression lessons.
### In the population data frame, some country names have annotations at the end.
### Using regular expressions, remove all the annotations in country names. (e.g. change "China[a]" to "China") (4 pts)
### (Hint: Combine apply family functions and string split methods), then convert the country names to uppercase. (1 pt)
### Show the first 5 rows of your new population data frame. (1 pt)
### Your code here
population[,1]=gsub("\\[.*", "", population[,1])
population[,1]=toupper(population[,1])
head(population,n=5)

### Q6. Now merge LatLon with population by country to create a data frame named AllData. (2 pts)
### It should have 6 columns: Country, Code, Latitude, Longitude, Population_2016 and Population_2017.

### Your code here
AllData=merge(LatLon,population,by="Country")
head(AllData)


### Q7. Finally, convert the population data to numeric values, and
### calculate the 2016-2017 growth rate percentage of population by country,
### and add the growth rate to AllData as a new column named "Growth". (3 pts)
### (Hint: growth rate percentage = (population in 2017 / population in 2016 - 1)*100)
### Show the last 5 rows of your AllData data frame. (1 pt)

### Your code here
AllData$Population_2016=as.numeric(gsub(",", "", AllData$Population_2016))
AllData$Population_2017=as.numeric(gsub(",", "", AllData$Population_2017))
AllData$Growth=(AllData$Population_2017/AllData$Population_2016 -1)*100

### Part II.  Create a KML document for google earth visualization 

### First take a look at the file on moodle: HW8_Intro.pdf
### It shows the structure of the KML file which we will create next.

### Q8. Let's start with creating the base of the KML document.
### Create a base document named doc1. (1 pt)
### Then create nodes "kml" and "document". (2 pts)
### (Hint: Check arguments "doc" and "parent" to make the nodes connected)

### Your code here
doc1=newXMLDoc()
kml=newXMLNode('kml',doc = doc1)
document=newXMLNode('Document',parent = kml)
doc1

### Q9. According to the KML tree in HW8_Intro.pdf, you can add many placemark nodes with parent "Document".
### The addPlacemark() function below can be used to add one placemark to your file in each call.
### Explain what each line does. (4 pts)

addPlacemark = function(lat,lon,country,code,pop16,pop17,growth,parent){
  pm = newXMLNode("Placemark",attrs=c(id=code),parent=parent)
  newXMLNode("name",country,parent=pm)
  newXMLNode("description",paste(country,"\n population_2016: ",pop16,"\n population_2017: ",pop17,"\n growth: ",growth,sep =""),parent=pm)
  newXMLNode("Point",newXMLNode("coordinates",paste(c(lon,lat,0),collapse=",")),parent=pm)
}

### Your answer here
#line1: Specify the name and input of a function 

#line2: Create a node name "Placemark" with name-value pairs (id=code) to be used as attributes and specify 
#the parent of that node(parent=parent), and we name this node in R as "pm" which is not part of the resulting XML document;

#line3: Create a node name "name" whose parent is the node we previous created ("pm" in R, "Placemark" in resulting XML ducument);

#line4: Create a node name "description" whose content is the counry and the corresponding population in 2016 and 2017 and the corresponding growth rate. 
#Specify the parent node is the previous node we made("pm" in R, "Placemark" in resulting XML ducument)

#line5: Create a node name "Point" with a child node name "coordinates" whose content is the "c(lon,lat,0)". Specify the parent
#node is the previous node we made("pm" in R, "Placemark" in resulting XML ducument)



### Q10. Now let's create the KML file.
### Take doc1 as your base, then use addPlacemark() to create a KML file. (5 pts)
### Save it as "Part2.kml" using the saveXML() function. (1 pt)
### (Hint: First find the root of your base, then add placemark nodes to it in a for loop)
### Open the KML document in Google Earth. (You will need to install Google Earth.) 
### If you are doing correctly, it should have pushpins for all the countries.

### Your code here

for(i in 1:nrow(AllData)){
addPlacemark(lat=AllData$Latitude[i],lon=AllData$Longitude[i],country=AllData$Country[i],
             code=AllData$Code[i],pop16=AllData$Population_2016[i],pop17=AllData$Population_2017[i],
             growth=AllData$Growth[i],parent=document)
}

saveXML(doc1,file = "Part2.kml")

### Part III.  Add Style to your KML file (16 pts)

### Now you are going to make the visualization a bit fancier.
### Instead of pushpins, we want different labels for countries with different population sizes in 2017,
### and we will use different colors to represent different levels of population growth rate.
### Code is given to you below to create style elements.
### Here, you just need to figure out what it all does.

### Q11. The following code is an example of how to create cut points for different categories of population in 2016.
pop16Cut = as.numeric(cut(AllData$Population_2016, breaks=5))

### (But this example contains too many 1's, which is not suitable for visualization)
### So find suitable cut points for 2017 population and growth rate,
### and create your categories named pop17Cut and growCut. (3 pts)

### Hint: take a look at their distribution first. You may want to perform some simple transformations
### before finding the cuts.
### Explain the transformation you chose and why; also explain how you chose your cuts. (3 pt)

### Your code here
barplot(AllData$Population_2017)
barplot(log(AllData$Population_2017))
log_2017=log(AllData$Population_2017)

pop17Cut=as.numeric(cut(log_2017, 
                    breaks=c(quantile(log_2017, probs = seq(0, 1, by = 0.2))),
                    include.lowest=TRUE))
pop17Cut
#Growth rate
plot(AllData$Growth)
barplot(AllData$Growth)

growCut=as.numeric(cut(AllData$Growth, 
                        breaks=c(quantile(AllData$Growth, probs = seq(0, 1, by = 0.2))),
                        include.lowest=TRUE))

growCut
### Your answer here

#I chose to do log-transformation on Population_2017, because, as we can see, China and India have much larger value than others.
#In order to find appropriate cut points, applying log-transforamtion can reduce the variability of data and 
#make data conform more closely to the normal distribution, at the same time, keep the features of data;
#After applying log-transformation, the quantile of population makes more sense and I choose the quantile from 0 to 1 increases each time by 20% 
#as cut points to form intervals. So I end up with 5 intercals and check each population numer falls in which interval, 
#then express the corresponding intervals to number which can represent the population level.

#Since growth rate is not highly skewed data so that I didn't do transormation on data; The way I choose cut points of growth rate is the same as that of population.


### Q12. We modify the addPlacemark() function in Q8, so it can add both placemark and style information.
### It has 3 new arguments: pop17cut, growcut, and style.
### Explain what the new line of code does. (2 pts)

addPlacemark = function(lat,lon,country,code,pop16,pop17,growth,parent,pop17cut,growcut,style=TRUE){
  pm = newXMLNode("Placemark",newXMLNode("name",country),attrs=c(id=code),parent=parent)
  newXMLNode("description",paste(country,"\n population_2016: ",pop16,"\n population_2017: ",pop17,"\n growth: ",growth,sep =""),parent=pm)
  newXMLNode("Point",newXMLNode("coordinates",paste(c(lon,lat,0),collapse=",")),parent=pm)
  if(style){newXMLNode("styleUrl",paste("#YOR",growcut,"-",pop17cut,sep=''),parent=pm)}
}



### Your answer here

#If 'style' is True(which is default), create a node name "styleUrl" with content which is "#YOR corresponding growcut - pop17cut", and also specify 
#the parent of that node is "pm" in R ("Placemark" in resulting XML document).


### Q13. Here is another function addStyle(), by which we can add style information to KML file.
### Figure out what the arguments "scales" and "colors" should be, and
### create two objects scale_label and color_label that you can input into this function. (5 pts)
### (Hint: For growth rate from low to high, you want to use this order of color: blue-green-yellow-orange-red)
### (Hint2: make a bigger symbol for country with larger population)

### Your code here

addStyle = function(parent,scales,colors){
  for(j in 1:5){
    for(k in 1:5){
      st = newXMLNode("Style",attrs=c("id"=paste("YOR",j,"-",k,sep="")),parent=parent)
      newXMLNode("IconStyle",newXMLNode("Icon",paste("color_label/label_",colors[j],".png",sep="")),
                 newXMLNode("scale",scales[k]),parent=st)
    }
  }
}

scale_label=c(1,2,3,4,5)
color_label=c('blue','green','yellow','orange','red')

### Q14. Let's build a tree properly, that contains both country and style information. (6 pts)

### You can complete this by following steps:
### 1) Create a base KML document named "doc2", similar to what you did in Q8
### 2) Add style information by addStyle() in nested for loops
### 3) Add placemarks by addPlacemark()

### Your code here

doc2=newXMLDoc()
kml2=newXMLNode('kml',doc = doc2)
document2=newXMLNode('Document',parent = kml2)
doc2

addStyle(parent = document2,colors=color_label,scales=scale_label)

for(i in 1:nrow(AllData)){
  addPlacemark(lat=AllData$Latitude[i],lon=AllData$Longitude[i],country=AllData$Country[i],
               code=AllData$Code[i],pop16=AllData$Population_2016[i],pop17=AllData$Population_2017[i],
               growth=AllData$Growth[i],parent=document2,pop17cut=pop17Cut[i],growcut=growCut[i])
}


### Q15. Output your KML document, call it "Part3.kml". (1 pt)
### Open it in Google Earth to verify that it works.
### Explain your findings about the world population from the results you get. (2 pts)

### Your code here
saveXML(doc2,file = "Part3.kml")

### Your answer here

#With respect to the size of stars which represents the sacle of population, we can see there are quite a few countries in Aisa, Africa,
#and America and have rather large stars which represents large population; while the population in Oceania is relatively small;
#And with respect to the growth rate which representes by the color of the star, we can see that in general, countries in Afirca have
#higher growth rate; while Russia, America, Brazil, China and lots of contries in Europe have low growth rate;




### For this assignment, you only need to submit your R code and "Part3.kml", the PDF report is not required.
