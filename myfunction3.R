library(XML)

myfunction3 <- function(year, month) {
        date <- as.Date(paste(year,"-",month,"-","01",sep=""))
        from <- paste(year,"%2F",month,"%2F",1,sep="")
        to <- paste(year,"%2F",month,"%2F",numberOfDays(date),sep="")
        
        file_name <- paste("최종낙찰자_",year,".", month,".csv",sep="")
        
        ## Crawl all public procurement data from 나라장터
        pgvector <- c()
        for(i in 1:500){
                url <- paste("http://www.g2b.go.kr:8101/ep/tbid/integrationWbidderList.do?area=&areaNm=&bidNm=&bidSearchType=3&budget=&budgetCompare=UP&detailPrdnm=&detailPrdnmNo=&fromBidDt=",from,"&fromOpenBidDt=&industry=&industryCd=&instNm=&instSearchRangeType=&intbidYn=&procmntReqNo=&radOrgan=1&recordCountPerPage=100&refNo=&regYn=Y&searchDtType=1&searchType=1&taskClCds=&toBidDt=",to,"&toOpenBidDt=&useTotalCount=Y&currentPageNo=",i,sep="")
                html <- htmlTreeParse(url, useInternalNode = TRUE)
                if(xpathSApply(html, "//td", xmlValue)[1] != "검색된 데이터가 없습니다.")
                        pgvector <- c(pgvector, xpathSApply(html, "//td", xmlValue))
                else
                        break
        }
        
        ## Identity the index of first column of each rows
        ## And save these indexes to 'leftmost'
        v <- c("물품","공사","용역","리스","외자","비축","기타","민간")
        leftmost <- c()
        for(i in 1:length(pgvector)){
                if(pgvector[i] %in% v){
                        leftmost <- c(leftmost, i)
                }
        }
        
        ## In some cases, some data scraping were not successful
        ## Tag these error datas 
        ## In for loop condition, do not forget to use ()
        for(i in 2:length(leftmost)){
                if((leftmost[i] - leftmost[i-1]) != 12){
                        for(j in leftmost[i-1]:(leftmost[i]-1)){
                                pgvector[j] <- 'bug'
                        }
                }
        }
        
        ## Remove tagged data
        pgvector <- pgvector[pgvector != 'bug']
        
        ## Make matrix from vector
        pgmatrix_3_2015.1 <- matrix(pgvector, ncol = 12, byrow = TRUE)
        
        ## remove "\r", "\t", "\n" in data
        for(i in 1:length(pgmatrix_3_2015.1[ ,8])){
                x <- unlist(strsplit(pgmatrix_3_2015.1[i,8], split = character(0)))
                x <- x[(x != "\r") & (x != "\t") & (x != "\n")]
                pgmatrix_3_2015.1[i,8] <- paste(x, collapse="")
        }
        
        for(i in 1:length(pgmatrix_3_2015.1[ ,9])){
                x <- unlist(strsplit(pgmatrix_3_2015.1[i,9], split = character(0)))
                x <- x[(x != "\r") & (x != "\t") & (x != "\n")]
                pgmatrix_3_2015.1[i,9] <- paste(x, collapse="")
        }
        
        for(i in 1:length(pgmatrix_3_2015.1[ ,10])){
                x <- unlist(strsplit(pgmatrix_3_2015.1[i,10], split = character(0)))
                x <- x[(x != "\r") & (x != "\t") & (x != "\n")]
                pgmatrix_3_2015.1[i,10] <- paste(x, collapse="")
        }
        
        for(i in 1:length(pgmatrix_3_2015.1[,11])){
                x <- unlist(strsplit(pgmatrix_3_2015.1[i,11], split = character(0)))
                x <- x[(x != "\r") & (x != "\t") & (x != "\n")]
                pgmatrix_3_2015.1[i,11] <- paste(x, collapse="")
        }
        
        for(i in 1:length(pgmatrix_3_2015.1[ ,12])){
                x <- unlist(strsplit(pgmatrix_3_2015.1[i,12], split = character(0)))
                x <- x[(x != "\r") & (x != "\t") & (x != "\n")]
                pgmatrix_3_2015.1[i,12] <- paste(x, collapse="")
        }
        
        ## Make data.frame from vector
        pgdataframe_3_2015.1 <- data.frame(pgmatrix_3_2015.1)
        
        ## Assign variable names to data.frame
        colnames(pgdataframe_3_2015.1) <- c("업무","입찰공고번호","재입찰","공고명","수요기관","개찰일시","참가수","낙찰자","낙찰금액","낙찰률(%)","비고","링크")
        
        ## Write csv file from data.frame
        write.csv(file=file_name, x=pgdataframe_3_2015.1)
}
