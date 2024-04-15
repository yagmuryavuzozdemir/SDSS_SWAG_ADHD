adhd = read.csv("train_data.csv")
adhd = t(adhd)
colnames(adhd) = as.character(adhd[2,])
adhd = adhd[-(1:2),]
X = apply(adhd[,-1], 2, as.numeric)
adhd = data.frame(adhd[,1], X)
adhd = as.data.frame(adhd)

rownames(adhd) = 1:nrow(adhd)

adhd[adhd == "Controls"]=0
adhd[adhd == "ADHD-C" | adhd == "ADHD-H" | adhd == "ADHD-I"]=1

predictors = as.data.frame(adhd[,-1])
y = as.factor(adhd[,1])

set.seed(2705) 
ind = sample(1:dim(predictors)[1],dim(predictors)[1]*0.2)  
y_test = y[ind]
y_train = y[-ind]
x_test = predictors[ind,]
x_train = predictors[-ind,]

load("ADHD_package.Rda")

min_error = min(train_swag_svml$cv_alpha)


varmat_ind = list() #saves the varmat index from CV errors
for(i in 1:20){
  varmat_ind[[i]]=which(train_swag_svml$CVs[[i]]<=min_error, arr.ind = T)
}

post_sel = list() # models selected after post-processing
for(i in 1:20){
  post_sel[[i]] = train_swag_svml$VarMat[[i]][,varmat_ind[[i]]]
}

post_sel[[1]] = t(as.matrix(post_sel[[1]]))

for(i in 1:20){
  if(!is.matrix(post_sel[[i]])){
    post_sel[[i]]=t(as.matrix(post_sel[[i]]))
  }
}

post_sel[[9]] = t(post_sel[[9]])


x = c() #non-empty elements of post-group
for(i in 1:20){
  if(length(post_sel[[i]])!=0){
    x = c(x,i)
  }
  x = x
}

for(i in 1:length(x)){
  if(nrow(post_sel[[x[i]]])<20){
    diff = 20 - nrow(post_sel[[x[i]]])
    post_sel[[x[i]]] = rbind(post_sel[[x[i]]],matrix(NA,nrow = diff, ncol = ncol(post_sel[[x[i]]])))
  } 
}


models = matrix(NA, nrow = 20,ncol=ncol(post_sel[[x[1]]]))

models[1:nrow(post_sel[[x[1]]]),]=post_sel[[x[1]]]

for(i in 2:length(x)){
  models = cbind(models,post_sel[[x[i]]])
}

models = t(models)

selected_var = c() 
for(i in 1:ncol(models)){
  selected_var = c(selected_var,models[,i])
}
selected_var = na.omit(unique(selected_var))
selected_var = sort(selected_var)
#colnames(x_train)[selected_var]
#table(models)

freq = table(models)
variable = colnames(x_train)[selected_var]

freq_table = cbind(variable,freq)
rownames(freq_table) = c(1:nrow(freq_table))
freq_table = as.data.frame(freq_table)
freq_table$freq = as.numeric(freq_table$freq)

freq_ordered = freq_table[order(freq_table$freq, decreasing = T),]
freq_ordered_trimmed = freq_ordered[1:16,]

write.csv(freq_ordered, file = "freq_ordered.csv")

most_index = which(freq_table$freq>200, arr.ind = T)

### adhd_network_data: relative frequency and sign of the coefficients of each feature

betas = matrix(NA, ncol=ncol(models), nrow=nrow(models))
for(i in 1:nrow(models)){
  ind_models = models[i,]
  ind_models = unlist(na.omit(ind_models))
  data = as.data.frame(cbind(y_train, x_train[,ind_models]))
  names(data) = c("y_train", names(x_train[,ind_models]))
  betas[i,1:length(ind_models)]=glm(y_train~ ., data = data, family = "binomial")$coefficients[-1]
}


sign_betas = list()
for(i in 1:length(selected_var)){
  sign_betas[[i]] = betas[which(models==selected_var[i],arr.ind = T)]
}

median_betas = rep(NA, length(sign_betas))

for(i in 1:length(median_betas)){
  median_betas[i] = median(sign_betas[[i]])
}

neg_beta = rep(NA,dim(freq_table)[1])
pos_beta = rep(NA,dim(freq_table)[1])

for(i in 1:length(sign_betas)){
 if(sum(sign(sign_betas[[i]]))==length(sign_betas[[i]])){
   pos_beta[i]=1
   neg_beta[i]=0
 }
 if(sum(sign(sign_betas[[i]]))==-length(sign_betas[[i]])){
   neg_beta[i]=1
   pos_beta[i]=0
 }
 if(sum(sign(sign_betas[[i]]))!=length(sign_betas[[i]]) & sum(sign(sign_betas[[i]]))!=-length(sign_betas[[i]])){
   neg_beta[i]=as.data.frame(table(sign(sign_betas[[i]])))[1,2]/sum(as.data.frame(table(sign(sign_betas[[i]])))[,2])
   pos_beta[i]=as.data.frame(table(sign(sign_betas[[i]])))[2,2]/sum(as.data.frame(table(sign(sign_betas[[i]])))[,2])
 }
}



percentage = freq_table$freq/381
adhd_network_data = data.frame(freq_table$variable,percentage,neg_beta,pos_beta)

adhd_network_data = adhd_network_data[most_index,]

#### Relation_mat

A = matrix(0, nrow = ncol(models), ncol =ncol(models))
intensity = matrix(0, nrow = length(selected_var), ncol = length(selected_var))
b=0
a = list()
for(i in 1:(length(selected_var)-1)){
  for(j in (i+1):length(selected_var)){
    for(k in 1:(ncol(models)-1)){
      a[[i]]=which(models[,k]==selected_var[i])
      for(n in (k+1):(ncol(models))){
        A[k,n]=length(which(models[a[[i]],n]==selected_var[j]))
      }
    }
    intensity[i,j]=sum(A)
    intensity[j,i]=sum(A)
  }
}


A = matrix(0, nrow = ncol(models), ncol =ncol(models))
intensity = matrix(0, nrow = length(selected_var), ncol = length(selected_var))
b=0
a = list()
for(i in 1:(length(selected_var)-1)){
  for(j in (i+1):length(selected_var)){
    for(k in 1:(ncol(models)-1)){
      a[[i]]=which(models[,k]==selected_var[i])
      for(n in (k+1):(ncol(models))){
        A[k,n]=length(which(models[a[[i]],n]==selected_var[j]))
      }
    }
    intensity[i,j]=sum(A)
    intensity[j,i]=sum(A)
  }
}



corr = matrix(NA, nrow = length(selected_var), ncol = length(selected_var))
for(i in 1:length(selected_var)){
  for(j in 1:length(selected_var)){
    corr[i,j] = cor(x_train[,selected_var[i]],x_train[,selected_var[j]], method = "spearman")
  }
}
colnames(corr) = colnames(x_train)[selected_var]
rownames(corr) = colnames(x_train)[selected_var]

corr = corr[most_index,most_index]

library(gdata)

relation_mat = matrix(NA, nrow = choose(16,2), ncol = 3)
#relation_mat[,3]=upperTriangle(corr, byrow = T)
c = rep(colnames(corr)[1],15)
for(i in 14:1){
  a = rep(colnames(corr)[16-i],i)
  c = c(c,a)
}
relation_mat[,1]=c

b = colnames(corr)[-1]
for(i in 3:ncol(corr)){
  b1 = colnames(corr)[i:16]
  b = c(b,b1)
}
relation_mat[,2]=b

intensity = intensity[most_index,most_index]
relation_mat[,3] = upperTriangle(intensity,byrow = T)/381
### med_beta


med_beta = matrix(NA, nrow = length(most_index),ncol = 2)
med_beta[,1]=colnames(corr)
# for(i in seq_along(most_index)){
#   if(median(sign_betas[[i]])>0){
#     med_beta[i,2]="#ACD6B0"
#   }else{
#     med_beta[i,2]="#C2A5CF"
#   }
# }

k=rep(NA, 16)
for(i in 1:16){
  k[i]=which(med_beta[i,1]==variable)
}

for(i in 1:16){
  med_beta[i,2]=median_betas[k[i]]
}

for(i in 1:16){
  if(med_beta[i,2]<0){
    med_beta[i,2]="#ACD6B0"
  }else{
    med_beta[i,2]="#C2A5CF"
  }
}


# #variables that are mostly together
# which(intensity==max(intensity),arr.ind = T)

### mat_col

mat_col = matrix(NA, nrow = choose(9,2), ncol = 3)

x = which(adhd_network_data$percentage>0.75, arr.ind = T)
y = expand.grid(x,x)
y = unique(t(apply(y,1,sort)))  
id_ndup = which(apply(y,1,anyDuplicated) == 0)
y = y[id_ndup,]
mat_col[,1:2]=y

for(i in 1:nrow(mat_col)){
  mat_col[i,3]=corr[mat_col[i,1],mat_col[i,2]]
}


### use the following for colors
fun_color_range = colorRampPalette(colors = c("red", "white", "blue"))
my_colors = fun_color_range(36)

mat_col = mat_col[order(mat_col[,3],decreasing = T),]
mat_col[,3] = my_colors
e = as.numeric(mat_col[,1])
mat_col[,1] = colnames(corr)[e]
f = as.numeric(mat_col[,2])
mat_col[,2] = colnames(corr)[f]




relation_mat = as.data.frame(relation_mat)
mat_col = as.data.frame(mat_col)

relation_mat[,3]=as.numeric(relation_mat[,3])

colnames(adhd_network_data) = c("feature","percentage","neg_beta","pos_beta")
colnames(med_beta) = c("feature","col_med_beta")
colnames(relation_mat) = c("X1", "X2", "rel_strength")
colnames(mat_col) = c("","","col_corr")

save(relation_mat, file = "relation_mat.Rda")
save(adhd_network_data, file = "adhd_network_data.Rda")
save(med_beta, file = "med_beta.Rda")
save(mat_col, file = "mat_col.Rda")


#### NETWORK


library(dplyr)
library(Hmisc)
library(igraph)
library(RColorBrewer)
library(stringr)
library(extrafont)


loadfonts() #This registers fonts so that they can be used with the pdf, postscript, or Windows bitmap output device. It must be run once in each R session.

MAX_EDGE_WIDTH = 10
############################################################################################################################################
###################################################### FULL NETWORK ########################################################################
############################################################################################################################################

## Remove ebv

load("relation_mat.Rda")
load("adhd_network_data.Rda")
load("mat_col.Rda")
load("med_beta.Rda")



# Color of edges
index = rep(NA,nrow(relation_mat))
for(i in 1:nrow(relation_mat)){
  i1 = which(relation_mat[i,1] == mat_col[,1] & relation_mat[i,2] == mat_col[,2])
  i2 = which(relation_mat[i,1] == mat_col[,2] & relation_mat[i,2] == mat_col[,1]) #works like sum of TRUE's, weird
  if(length(c(i1,i2))==0){
    index[i] = NA
  }else{
    index[i] = c(i1,i2)
  }
}



# Color of vrtex based on median ###### HOW TO GET NA HERE????
index_med = rep(NA,nrow(adhd_network_data))
for(i in 1:nrow(adhd_network_data)){
  if(length(which(adhd_network_data[i,1] == med_beta[,1])) == 0){
    index_med[i] = NA
  }else{
    index_med[i] = which(adhd_network_data[i,1] == med_beta[,1])
  }
}


relation_mat = data.frame(relation_mat,directed = F)
col_edge = mat_col[index,3]
col_edge[is.na(col_edge)] = "#F0F8FF"

## Duplicate relation mat
relation_mat_dup = relation_mat
# Remove hsa-mir from name
relation_mat_dup[,1] = gsub("X.", "", relation_mat[,1])
relation_mat_dup[,2] = gsub("X.", "", relation_mat[,2])
#relation_mat_dup[,1] = gsub(".*t-", "", relation_mat_dup[,1])
#relation_mat_dup[,2] = gsub(".*t-", "", relation_mat_dup[,2])


# for(i in 1:nrow(relation_mat_dup)){
#   if(relation_mat_dup[i,1]=="47.21."){
#     relation_mat_dup[i,1]="O"
#   }
#   if(relation_mat_dup[i,1]=="54.21."){
#     relation_mat_dup[i,1]="O"
#   }
#   if(relation_mat_dup[i,1]=="72.11."){
#     relation_mat_dup[i,1]="F"
#   }
#   if(relation_mat_dup[i,1]=="72.68."){
#     relation_mat_dup[i,1]="F"
#   }
#   if(relation_mat_dup[i,1]=="101.85."){
#     relation_mat_dup[i,1]="T"
#   }
#   if(relation_mat_dup[i,1]=="104.34."){
#     relation_mat_dup[i,1]="F"
#   }
#   if(relation_mat_dup[i,1]=="108.27."){
#     relation_mat_dup[i,1]="F"
#   }
#   if(relation_mat_dup[i,1]=="108.91."){
#     relation_mat_dup[i,1]="F"
#   }
#   if(relation_mat_dup[i,1]=="114.40."){
#     relation_mat_dup[i,1]="T"
#   }
#   if(relation_mat_dup[i,1]=="116.89."){
#     relation_mat_dup[i,1]="F"
#   }
#   if(relation_mat_dup[i,1]=="124.99."){
#     relation_mat_dup[i,1]="F"
#   }
#   if(relation_mat_dup[i,1]=="126.2."){
#     relation_mat_dup[i,1]="F"
#   }
#   if(relation_mat_dup[i,1]=="146.89."){
#     relation_mat_dup[i,1]="TF"
#   }
#   if(relation_mat_dup[i,1]=="150.114."){
#     relation_mat_dup[i,1]="T"
#   }
#   if(relation_mat_dup[i,1]=="151.147."){
#     relation_mat_dup[i,1]="O"
#   }
# }
# 
# for(i in 1:nrow(relation_mat_dup)){
#   if(relation_mat_dup[i,2]=="47.21."){
#     relation_mat_dup[i,2]="O"
#   }
#   if(relation_mat_dup[i,2]=="54.21."){
#     relation_mat_dup[i,2]="O"
#   }
#   if(relation_mat_dup[i,2]=="72.11."){
#     relation_mat_dup[i,2]="F"
#   }
#   if(relation_mat_dup[i,2]=="72.68."){
#     relation_mat_dup[i,2]="F"
#   }
#   if(relation_mat_dup[i,2]=="101.85."){
#     relation_mat_dup[i,2]="T"
#   }
#   if(relation_mat_dup[i,2]=="104.34."){
#     relation_mat_dup[i,2]="F"
#   }
#   if(relation_mat_dup[i,2]=="108.27."){
#     relation_mat_dup[i,2]="F"
#   }
#   if(relation_mat_dup[i,2]=="108.91."){
#     relation_mat_dup[i,2]="F"
#   }
#   if(relation_mat_dup[i,2]=="114.40."){
#     relation_mat_dup[i,2]="T"
#   }
#   if(relation_mat_dup[i,2]=="116.89."){
#     relation_mat_dup[i,2]="F"
#   }
#   if(relation_mat_dup[i,2]=="124.99."){
#     relation_mat_dup[i,2]="F"
#   }
#   if(relation_mat_dup[i,2]=="126.2."){
#     relation_mat_dup[i,2]="F"
#   }
#   if(relation_mat_dup[i,2]=="146.89."){
#     relation_mat_dup[i,2]="TF"
#   }
#   if(relation_mat_dup[i,2]=="150.114."){
#     relation_mat_dup[i,2]="T"
#   }
#   if(relation_mat_dup[i,2]=="151.147."){
#     relation_mat_dup[i,2]="o"
#   }
#   if(relation_mat_dup[i,2]=="162.73."){
#     relation_mat_dup[i,2]="O"
#   }
# }

adhd_network_data = data.frame(adhd_network_data)
col_vertex = as.vector(med_beta[,2][index_med])
col_vertex[is.na(col_vertex)] = "#FFFFFF"
### Create graph
g = graph.data.frame(relation_mat_dup, directed=F) 
vertex_size = 16
par(mfrow = c(1,1),mar = c(0,0,0,0), oma = c(0,0,0,0))

plot(g)

# layout
l = layout_with_fr(g)
l = norm_coords(l)
V(g)$x = l[,1]
V(g)$y = l[,2]

radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}

plot(g)


#for(i in 1:dim(relation_mat)[1]){
  #if(relation_mat[i,3]>325){
#     relation_mat[i,3]=100
#   }
#   if(relation_mat[i,3]>258&relation_mat[i,3]<=325){
#     relation_mat[i,3]=75
#   }
#   if(relation_mat[i,3]>207&relation_mat[i,3]<=258){
#     relation_mat[i,3]=50
#   }
#   if(relation_mat[i,3]>149&relation_mat[i,3]<=207){
#     relation_mat[i,3]=25
#   }
# }








V(g)$name = c("M", "A", "G", "B", "C", "E", "O", "P", "K", "D", "J",  "I","N", "H", "F", "L")
V(g)$color = col_vertex
V(g)$shape = "circle"
V(g)$label = V(g)$name
V(g)$label.color = "black"
V(g)$label.font = 0.7
V(g)$label.loc = 0.7
#V(g)$label.family = "sans"
V(g)$label.dist = c(rep(0,16))
#V(g)$label.cex = c(1.25,1.25,1.25,1.25,.55,rep(.45,length(V(g)$name)-5))
V(g)$size = 2*(adhd_network_data$percentage/max(adhd_network_data$percentage))*vertex_size
V(g)$label.cex = V(g)$size/29
V(g)$label.degree = c(rep(pi/2,8),rep(pi,8))
# V(g)$label.degree[9] = 3*pi/2
# V(g)$label.degree[15] = 3*pi/2
# V(g)$label.degree[18] = 3*pi/2





# duplicate lines
edge_width = (as.numeric(relation_mat[,3]))*5
#edge_width = (as.numeric(relation_mat$rel_strength))*20
edge_width[edge_width > MAX_EDGE_WIDTH] = MAX_EDGE_WIDTH

col_edge[col_edge == "#F0F8FF"] = "#C0C0C0"

E(g)$color = col_edge
E(g)$width = edge_width
E(g)$curved = .2
E(g)$arrow.size = .5


library(dichromat)
# m = 200
# cols = rev(RColorBrewer::brewer.pal(10, name = "RdBu"))
# colfunc1 <- colorRampPalette(c(cols[2], cols[5]))
# colfunc2 <- colorRampPalette(c(cols[6], cols[9]))
# cols = rev(c(colfunc1(m/2),colfunc2(m/2)))

m = 200
# cols = rev(RColorBrewer::brewer.pal(10, name = "RdBu"))
# colfunc1 <- colorRampPalette(c(cols[2], cols[5]))
# colfunc2 <- colorRampPalette(c(cols[6], cols[9]))
fun_color_range = colorRampPalette(colors = c("blue", "white", "red"))
cols = fun_color_range(m)


#library(tikzDevice)
#tikz("C:/Users/yagmu/Box/Auburn/Research/AL-MS Chapter presentation/graph.tex", width = 8, height = 5, standAlone = TRUE,
     #packages = c("\\usepackage{tikz}",
                  # "\\usepackage[active,tightpage,psfixbb]{preview}",
                  # "\\PreviewEnvironment{pgfpicture}",
                  # "\\setlength\\PreviewBorder{0pt}",
                  # "\\usepackage{amssymb}",
                  # "\\usepackage{bm}","\\usepackage{amsthm}","\\usepackage{amsbsy}"
                  # ,"\\usepackage{amsbsy}"
                  # ,"\\usepackage{amsbsy}"
                  # ,"\\usepackage{amsfonts}"))

## Legend
set.seed(1)
plot(g, rescale = T)
## Legend
## Legend
cex = .85
## Legend correlation
legend_corr = as.raster(matrix(rev(cols), ncol = 1))
text(x=0.9,y=1.1,"Spearman correlation" ,xpd=NA, pos = 4,cex=cex)
text(x=0.9,y=1.0, "between features",xpd=NA, cex = cex, pos = 4)
text(x=1.15, y = seq(-.1,.9,l=5), labels = seq(-1,1,l=5),cex=cex)
rasterImage(legend_corr, xleft = 1.25, ybottom = -.125, xright = 1.35,ytop = .925)

# Legend Beta
#legend(x = -1.6, y = -.85,legend=c("Positive $\\hat{\\beta}$","Negative $\\hat{\\beta}$"),fill=unique(col_vertex),bty = "n", cex = cex)

legend(x = -1.7, y = -.7,legend=c("Negative coefficient","Positive coefficient"),fill=unique(col_vertex),bty = "n", cex = cex)

# Legend Circle
text(x=-1.7,y=1.1,"Percentage of models",xpd=NA, cex = cex, pos = 4)
text(x=-1.7,y=1.0,"including the feature",xpd=NA, cex = cex, pos = 4)

lab_inc = c(expression(paste("[100%, 75%)")),
            expression(paste("[75%, 50%)")),
            expression(paste("[50%, 25%)")),
            expression(paste("[25%, 0%]")))
y = c(0.1,0.25,0.47,0.8)
cex = seq(.8,8,l = 4)
for (i in 1:length(lab_inc)){
  points(-1.75, y[i], xpd=NA, pch = 1, cex = (cex)[i])
  text(x=-1.65,y = y[i], rev(lab_inc)[i],xpd=NA, cex = cex, pos = 4)
}

# Legend Line
lab_inc = c(expression(paste("[100%, 75%)")),
            expression(paste("[75%, 50%)")),
            expression(paste("[50%, 25%)")),
            expression(paste("[25%, 0%]")))
y = seq(-.2,.8,l = 4)

text(x=1.2,y=-.4,"Link intensity ",xpd=NA, cex = cex, pos = 4)
text(x=1.2,y=-.5,"between pairs",xpd=NA, cex = cex, pos = 4)
text(x=1.2,y=-.6,"of features:",xpd=NA, cex = cex, pos = 4)
y = seq(-.7,-1.1,l = 4)
lwd = seq(.8,10,l = 4)

for (i in 1:length(lab_inc)){
  lines(x = c(1,1.1), c(y[i],y[i]), xpd=NA, pch = 1, lwd = rev(lwd)[i])
  text(x=1.2,y = y[i], (lab_inc)[i],xpd=NA, lwd = 1, pos = 4, cex = cex)
}


dev.off()



############################################################################################################################################
###################################################### NETWORK 1274a ######################################################################
############################################################################################################################################

gene = "200c"

i1 = which(relation_mat_dup[,1] == gene)
i2 = which(relation_mat_dup[,2] == gene)

single_gene = relation_mat_dup[c(i1,i2),]
single_gene = single_gene[-which(single_gene$rel_strenght == 0),]

index_other = c(single_gene$X2[1:5],single_gene$X1[6:9],gene)

index_pair = t(combn(index_other,2))

index = rep(NA,nrow(index_pair))

for(i in 1:nrow(index_pair)){
  
  tamer = which(relation_mat_dup$X1 == index_pair[i,1] & relation_mat_dup$X2 == index_pair[i,2])
  if(length(tamer) != 0){
    index[i] = tamer
  }
}

index = na.omit(index)


net_gene_200c = relation_mat_dup[index,]
net_gene_200c = net_gene_200c[-which(net_gene_200c$rel_strenght == 0),]

gene_network_data_375 = adhd_network_data


single_gene_mat = single_gene
col_edge = col_edge[c(i1,i2)]

### Create graph
g = graph.data.frame(single_gene_mat, directed=F)
vertex_size = 40
par(mfrow = c(1,1),mar = c(0,0,0,0), oma = c(0,0,0,0))

# layout
l = layout_with_fr(g)
l = norm_coords(l)
V(g)$x = l[,1]
V(g)$y = l[,2]

V(g)$color = col_vertex[-c(8,16)]
V(g)$shape = "circle"
V(g)$label = V(g)$name
V(g)$label.color = "black"
V(g)$label.dist = c(rep(0,5),-1.5,-1.5,rep(1,length(V(g)$name)-7))
V(g)$label.font = 1
V(g)$label.loc = 1
V(g)$label.cex = c(1,1,.8,.7,.7,rep(.7,length(V(g)$name)-5))
V(g)$label.degree = c(rep(pi/2,5),pi/2,rep(pi/2,length(V(g)$name)-6))
V(g)$size = (gene_network_data_375$percentage/max(gene_network_data_375$percentage))*vertex_size

# duplicate lines

edge_width = (single_gene_mat$rel_strenght)*50
edge_width[edge_width > MAX_EDGE_WIDTH] = MAX_EDGE_WIDTH

E(g)$color = col_edge[-c(18,19,20)]
E(g)$width = edge_width
E(g)$curved = .2
E(g)$arrow.size = .5
plot(g)
#############


library(tikzDevice)
tikz("tex/network_375.tex", width = 8, height = 5, standAlone = TRUE,
     packages = c("\\usepackage{tikz}",
                  "\\usepackage[active,tightpage,psfixbb]{preview}",
                  "\\PreviewEnvironment{pgfpicture}",
                  "\\setlength\\PreviewBorder{0pt}",
                  "\\usepackage{amssymb}",
                  "\\usepackage{bm}","\\usepackage{amsthm}","\\usepackage{amsbsy}"
                  ,"\\usepackage{amsbsy}"
                  ,"\\usepackage{amsbsy}"
                  ,"\\usepackage{amsfonts}"))

## Legend
plot(g, rescale = T)
## Legend
## Legend
cex = .85
## Legend correlation
legend_corr = as.raster(matrix(rev(cols), ncol = 1))
text(x=1.4,y=1.1,"Linear correlation $\\hat{\\rho}$" ,xpd=NA, pos = 4,cex=cex)
text(x=1.4,y=1.05, "between mi-RNAs:",xpd=NA, cex = cex, pos = 4)
text(x=1.7, y = seq(-.1,.9,l=5), labels = seq(-1,1,l=5),cex=cex)
rasterImage(legend_corr, xleft = 1.8, ybottom = -.125, xright = 1.9,ytop = .925)

# Legend Beta
legend(x = -2.3, y = -.7,legend=c("Positive $\\hat{\\beta}$","Negative $\\hat{\\beta}$"),fill=unique(col_vertex[c(1,3)]),bty = "n", cex = cex)

# Legend Circle
text(x=-2.3,y=1.1,"Percentage of model",xpd=NA, cex = cex, pos = 4)
text(x=-2.3,y=1.05,"including mi-RNA:",xpd=NA, cex = cex, pos = 4)

lab_inc = c("[100\\%, 85\\%)",
            "[85\\%, 60\\%)",
            "[60\\%, 45\\%)",
            "[45\\%, 30\\%)",
            "[30\\%, 15\\%)",
            "[15\\%, 0\\%]")
y = seq(-.2,.8,l = 6)
cex = seq(.8,4,l = 6)
for (i in 1:length(lab_inc)){
  points(-2.1, y[i], xpd=NA, pch = 1, cex = (cex)[i])
  text(x=-2,y = y[i], rev(lab_inc)[i],xpd=NA, cex = cex, pos = 4)
}

# Legend Line
lab_inc = c("[100\\%, 75\\%)",
            "[75\\%, 50\\%)",
            "[50\\%, 25\\%)",
            "[25\\%, 0\\%]")
y = seq(-.2,.8,l = 6)


text(x=1.4,y=-.7,"Link intensity ",xpd=NA, cex = cex, pos = 4)
text(x=1.4,y=-.8,"between pairs mi-RNA:",xpd=NA, cex = cex, pos = 4)
y = seq(-1,-1.4,l = 4)
lwd = seq(.8,10,l = 4)

for (i in 1:length(lab_inc)){
  lines(x = c(1.6,1.7), c(y[i],y[i]), xpd=NA, pch = 1, lwd = rev(lwd)[i])
  text(x=1.8,y = y[i], (lab_inc)[i],xpd=NA, lwd = 1, pos = 4, cex = cex)
}



dev.off()



