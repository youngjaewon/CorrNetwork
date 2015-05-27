library(XML)

myfunction1 <- function(year, month){
        date <- as.Date(paste(year,"-",month,"-","01",sep=""))
        from <- paste(year,"%2F",month,"%2F",1,sep="")
        to <- paste(year,"%2F",month,"%2F",numberOfDays(date),sep="")
        
        file_name <- paste("입찰공고_",year,".", month,".csv",sep="")
        
        pgvector <- c()
        
        for(i in 1:500){
                url <- paste("http://www.g2b.go.kr:8101/ep/tbid/tbidList.do?area=&areaNm=&bidNm=&bidSearchType=1&budget=&budgetCompare=UP&detailPrdnm=&detailPrdnmNo=&fromBidDt=",from,"&fromOpenBidDt=&industry=&industryCd=&instNm=&instSearchRangeType=&intbidYn=&procmntReqNo=&radOrgan=1&recordCountPerPage=100&refNo=&regYn=Y&searchDtType=1&searchType=1&taskClCds=&toBidDt=",to,"&toOpenBidDt=&useTotalCount=Y&currentPageNo=",i,sep="")
                html <- htmlTreeParse(url, useInternalNode = TRUE)
                if(xpathSApply(html, "//td", xmlValue)[1] != "검색된 데이터가 없습니다.")
                        pgvector <- c(pgvector, xpathSApply(html, "//td", xmlValue))
                else
                        break
        }
        
        v <- c("물품","공사","용역","리스","외자","비축","기타","민간")
        
        leftmost <- c()
        
        for(i in 1:length(pgvector)){
                if(pgvector[i] %in% v){
                        leftmost <- c(leftmost, i)
                }
        }
        
        ## In for loop condition, do not forget to use ()
        for(i in 2:length(leftmost)){
                if((leftmost[i] - leftmost[i-1]) != 10){
                        for(j in leftmost[i-1]:(leftmost[i]-1)){
                                pgvector[j] <- 0
                        }
                }
        }
        
        pgvector <- pgvector[pgvector != 0]
        pgmatrix_1_2015.2 <- matrix(pgvector, ncol = 10, byrow = TRUE)
        pgdataframe_1_2015.2 <- data.frame(pgmatrix_1_2015.2)
        colnames(pgdataframe_1_2015.2) <- c("업무","공고번호-차수","분류","공고명","공고기관","수요기관","계약방법","입력일시(입찰마감일시)","공동수급","투찰")
        
        write.csv(file=file_name, x=pgdataframe_1_2015.2)
        
}
