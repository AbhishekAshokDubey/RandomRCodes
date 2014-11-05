library(XML)
storyCount = 0
storyContent = c()
for(j in 1:456){ 
    storyURLs = c()
    link = url(paste0("http://longform.org/posts?page=",j))
    webpage = readLines(link)
    close(link)
    #b = grep("http://www",webpage)
    print(paste0("We are on the page no: ",j))
    pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)
    storyURLs = unlist(xpathApply(pagetree, "//div[@class='title']/a[@href]",xmlGetAttr, "href"))
    for(i in 1:length(storyURLs))
    {
        storyCount = storyCount + 1
        print(storyURLs[i])
        storyContent[storyCount] = tryCatch({
            link = url(storyURLs[i])
            webpage = readLines(link)
            close(link)
            pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)
            paste(unlist(xpathApply(pagetree, "//p", xmlValue)),sep="",collapse=" ")            
        }, warning = function(warningcondition) {            
            print(warningcondition)
            link = url(storyURLs[i])
            webpage = readLines(link)
            close(link)
            pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE) 
            paste(unlist(xpathApply(pagetree, "//p", xmlValue)),sep="",collapse=" ")            
        }, error = function(errorcondition) {
            print(errorcondition)
        }, finally={})
    }    
}

storyID = seq_along(storyContent)
storyFrame = as.data.frame(cbind(storyID,unlist(storyContent)))
names(storyFrame) = c("Id","Story")
write.csv(storyFrame,"StorySummarySecond.csv")

