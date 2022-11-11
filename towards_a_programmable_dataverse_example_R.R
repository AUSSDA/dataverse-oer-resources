# Towards a programmable Dataverse
# Finding, getting and analysing the Data you need

# install.packages("dataverse", "tibble", "ggplot2") # in case you have not installed these packages yet

library(dataverse)
library(tibble)
library(ggplot2)

# Step 1: Define and connect to repository API
repository_url <- "https://data.aussda.at/"
Sys.setenv("DATAVERSE_SERVER" = repository_url) 

# Step 2: Define dataset and retrieve dataset
DOI <- "doi:10.11587/EHJHFJ"
dataset <- get_dataset(DOI)

# Downloading Data: list files
files_list <- dataset$files[c("filename", "contentType")]

# Downloading Data: identify desired file
filename <- files_list$filename
ext <- unlist(strsplit(filename, "\\."))[c(F,T)]
ident <- filename[which(ext %in% "tab")][1] # save the filename of the first .tab file
\end{lstlisting}

# Downloading Data: download data
data <- get_dataframe_by_name(
  paste(ident),
  paste(DOI),
)

# Show sample data by displaying the first rows
head(data)

# Plotting Data
# recode missings and "cannot say" as missing values
# -99 = respondent has not worked before
# 8 = cannot say
data$bf29_7[data$bf29_7==-99] <- NA 
data$bf29_7[data$bf29_7==8] <- NA 

# Plotting Data
boxplot(bf29_7~SEX_2016,data=data,
        horizontal=TRUE,
        names=c("male","female"),
        col=c("wheat","thistle"),
        xlab="1=very important, 5=not important at all 
        (without cannot say)",
        ylab="Gender",
        main="Importance of societal relevance of one's job")

# Plotting Data
# ggplot for bar plot by groups
# define facet labels
to_string <- as_labeller(c(`1` = "male", `2` = "female"))

# plot
myplot <- ggplot(data=data, aes(x= bf29_7,  group=SEX_2016)) + 
        geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
        scale_y_continuous(labels=scales::percent) +
        labs(y = "Percent", 
             x="1=very important, 5=not important at all, without cannot say",
             title="Importance of societal relevance of one's job", 
             fill="Wichtigkeit") +
        guides(fill=FALSE)+
        facet_grid(.~SEX_2016, labeller = to_string)
myplot

# Downloading data manually by accessing a file directly
data <- get_dataframe_by_name(
  "10007_da_de_v1_2-1.tab",
  paste(DOI))
# and continue with head(data) etc