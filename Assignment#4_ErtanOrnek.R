# Jump-Start Code for Data Visualization PREDICT 455
# Individual Assignment 4: Visualizing Networks 

# set working directory
setwd("D:/Backup/Dropbox/Canvas/455/week7/Assignment4")

# install needed packages if they aren't already 
# install.packages("igraph")
# install.packages("network")
# install.packages("intergraph")

# bring in packages we rely upon for work in predictive analytics
library(igraph)  # network/graph methods
library(network)  # network representations
library(intergraph)  # for exchanges between igraph and network
library(svglite)
# ----------------------------------------------------------
# Preliminary note about data preparation
#


# ----------------------------------------------------------
# Read in list of links... (from-node, to-node) pairs
# ----------------------------------------------------------
all_links <- read.table('sampleemails.csv', header = TRUE,sep=",")
cat("\n\nNumber of Links on Input: ", nrow(all_links))
# check the structure of the input data data frame
#all_links<-all_links[c('V1','V2')]


print(str(all_links))
weekdays=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")

#Create Graphs Per Weekday based on Manager (15)
for (item in seq(1:7)){
print (item)
# ensure that no e-mail links are from to himself/herself
# i.e. eliminate any nodes that are self-referring 
all_valid_links <- subset(all_links, subset = (V1 != V2))
all_valid_links<-subset(all_valid_links,subset=(V2!=17 & V1!=17 &(V1 != V2) & (Week==item) ))

cat("\n\nNumber of Valid Links: ", nrow(all_valid_links))
all_valid_links<-aggregate(cbind(Week, Year) ~ V1 + V2, all_valid_links, length)
all_valid_links<-all_valid_links[order(-all_valid_links$Week),]
#pick top 20 communications
all_valid_links<-all_valid_links[1:20,]

#store node sizes to represent communication frequency
node.size<-aggregate(cbind(Week) ~ V1, all_valid_links, sum)
node.size2<-aggregate(cbind(Week) ~ V2, all_valid_links, sum)
names(node.size2)[1]='V1'
node.size<-rbind(node.size,node.size2)
node.size<-aggregate(cbind(Week) ~ V1, node.size, sum)
node.size$Week<-node.size$Week/max(node.size$Week)
node.size$Week<-node.size$Week*20.0+25.0

#Let's simply plot the links
company_net <- network(as.matrix(all_valid_links),
                     matrix.type = "edgelist", directed = TRUE, multiple = TRUE)
# create graph object with intergraph function asIgraph()
company_graph <- asIgraph(company_net)

# set up node reference table/data frame for later subgraph selection
node_index <- as.numeric(V(company_graph))
V(company_graph)$name <- node_name <- as.character(V(company_graph))
node_name <- as.character(node_index)
node_reference_table <- data.frame(node_index, node_name)

node.size.final<-setNames(node.size[,2],node.size[,1])


# consider the subgraph of all people that node "1"
# communicates with by e-mail (mail in or out)
ego_1_mail <- induced.subgraph(company_graph,
                               neighborhood(company_graph, order = 1, nodes = 15)[[1]])
# plot (ego_1_mail,edge.arrow.size = 0.04)

# examine alternative layouts for plotting the ego_1_mail 
#pdf(file = "How Main Manager Commmunicates.pdf", width = 5.5, height = 5.5)
par(mfrow = c(1,1))  # four plots on one page
set.seed(9999)  # for reproducible results

fname<-paste(weekdays[item],".svg",sep="")

print (fname)
svglite(file = fname, width = 4, height = 4,bg="transparent")


plot(ego_1_mail,  vertex.color = "gold1", vertex.size=as.matrix(node.size.final),
      edge.arrow.size = 0.14,edge.color = "burlywood4",vertex.frame.color="gold1",vertex.label.cex = 0.9,vertex.system_fonts=list("Calibri"),
     layout = layout.circle,vertex.label.color="black")
title(weekdays[item])  
dev.off()
dev.off()
}

############################################################################

library("d3Network")
library("networkD3")
library("rprojroot")
library("rmarkdown")


all_valid_links <- subset(all_links, subset = (V1 != V2))
all_valid_links<-subset(all_valid_links,subset=(V2!=17 & V1!=17 &(V1 != V2)  ))

cat("\n\nNumber of Valid Links: ", nrow(all_valid_links))
all_valid_links<-aggregate(cbind(Week, Year) ~ V1 + V2, all_valid_links, length)
all_valid_links<-all_valid_links[order(-all_valid_links$Week),]
#pick top 20 communications
all_valid_links<-all_valid_links[1:20,]


#store node sizes to represent communication frequency
node.size<-aggregate(cbind(Week) ~ V1, all_valid_links, sum)
node.size2<-aggregate(cbind(Week) ~ V2, all_valid_links, sum)
names(node.size2)[1]='V1'
node.size<-rbind(node.size,node.size2)
node.size<-aggregate(cbind(Week) ~ V1, node.size, sum)
node.size$Week<-node.size$Week/max(node.size$Week)
node.size$Week<-node.size$Week*20.0+25.0


library(qdapTools)


# Pull in json data generated from City of Chicago budget
# Data source = https://data.cityofchicago.org/Administration-Finance/Budget-2014-Budget-Ordinance-Appropriations/ub6s-xy6e/data/
# Json file prepared by K. Daugherty using https://shancarter.github.io/mr-data-converter/
budget <- paste0("chicago_budget.json")
chi_budget <- jsonlite::fromJSON(budget)



companyNames<-list("Michael Richardson","Benjamin Green","Russell Henderson","Peter James","Jacqueline Cooper","Scott Hall","Norma Rivera","Kathy Young","Dennis Perez","Alice Hernandez","Ruby Moore","Martin Williams","Brandon Harris","Laura Btry","Adam Roberts","James Wood","Justin Baker","Virginia Alexander","Katherine Griffin","Ann Coleman","Joseph Sanchez","Louis King","Kathryn Miller","Laura Gray","Ryan Gonzalez","Nicholas Stewart","Kenneth Ward","Arthur Hughes","Patrick White","Louise Morris","Joyce Russell","Bonnie Gonzales","Harry Kelly","Albert Torres","Douglas Patterson","Carlos Price","Emily Powell","Carl Murphy","Roger Howard","Thomas Clark","Clarence Turner","Linda Brooks","Teresa Collins",
                   "Jane Barnes","Walter Davis","Heather Robinson","Judith Bailey","Carol Bennett"
)
companyNames<-companyNames[node.size$V1]
companyNames<-list2df(companyNames,col1 = "name")
companyNames<-companyNames[c("name")]
all_valid_links$V1<-match(all_valid_links$V1,node.size$V1)
all_valid_links$V2<-match(all_valid_links$V2,node.size$V1)
all_valid_links$V1<-all_valid_links$V1-1
all_valid_links$V2<-all_valid_links$V2-1

all_valid_links<-all_valid_links[order(all_valid_links$V1,all_valid_links$V2),]
#names(companyNames)[1]="name"
# Examine the data to make sure it's ready for visualization

chi_budget$nodes<-companyNames 
#zero index 
# all_valid_links$V1<-all_valid_links$V1-1
# all_valid_links$V2<-all_valid_links$V2-1

all_valid_links<-all_valid_links[c("V1","V2","Week")]
names(all_valid_links)<-c("source","target","value")

all_valid_links$source<-as.integer(all_valid_links$source)
all_valid_links$target<-as.integer(all_valid_links$target)
all_valid_links$value<-as.numeric(all_valid_links$value)
chi_budget$links<-all_valid_links
# Plot sankey network diagram


links = as.data.frame(matrix(
  c(
         2,      0,   267,
         2,      1,   218,
         2,      3,   180,
         2,      4,   280,
         2,      5,   175,
         2,      6,   206,
         2,      7,   420,
         2,      9 ,  301,
         2,     10,   234,
        7,      0,   260,
        7,      2,   424,
        7,      4,   369,
        7,      5,   209,
        7,      8,   225,
        7,      9,   242
    #   9,      2,   350,
    #    9,      4,   301,
    #    9,      7,   292
        #11,      2,   378
 #       11,      9,   297
  
  ),# The third number is the value of the node
  byrow = TRUE, ncol = 3))
nodes=companyNames#
#rownames(all_valid_links) <- 1:nrow(all_valid_links)
#links=all_valid_links
names(links) = c("source", "target", "value")
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30,fontFamily = "Calibri")

