library(XML)

myfunction2 <- function(year, month){
        date <- as.Date(paste(year,"-",month,"-","01",sep=""))
        from <- paste(year,"%2F",month,"%2F",1,sep="")
        to <- paste(year,"%2F",month,"%2F",numberOfDays(date),sep="")
        
        file_name <- paste("개찰결과_",year,".", month,".csv",sep="")
        
        pgvector <- c()
        
        for(i in 1:500){
                url <- paste("http://www.g2b.go.kr:8101/ep/result/listPageIntegrationBidResult.do?area=&areaNm=&bidNm=&bidSearchType=2&budget=&budgetCompare=UP&detailPrdnm=&detailPrdnmNo=&fromBidDt=",from,"&fromOpenBidDt=&industry=&industryCd=&instNm=&instSearchRangeType=&intbidYn=&procmntReqNo=&radOrgan=1&recordCountPerPage=100&refNo=&regYn=Y&searchDtType=1&searchType=1&taskClCds=&toBidDt=",to,"&toOpenBidDt=&useTotalCount=N&currentPageNo=",i,sep="")
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
                if((leftmost[i] - leftmost[i-1]) != 11){
                        for(j in leftmost[i-1]:(leftmost[i]-1)){
                                pgvector[j] <- 'bug'
                        }
                }
        }
        
        pgvector <- pgvector[pgvector != 'bug']
        pgmatrix_2_2015.1 <- matrix(pgvector, ncol = 11, byrow = TRUE)
        
        for(i in 1:length(pgmatrix_2_2015.1[,11])){
                if(grepl("개찰완료", pgmatrix_2_2015.1[i,11]))
                        pgmatrix_2_2015.1[i,11] <- "개찰완료"
                if(grepl("유찰", pgmatrix_2_2015.1[i,11]))
                        pgmatrix_2_2015.1[i,11] <- "유찰"
                if(grepl("재입찰", pgmatrix_2_2015.1[i,11]))
                        pgmatrix_2_2015.1[i,11] <- "재입찰"
                if(grepl("분류조회", pgmatrix_2_2015.1[i,11]))
                        pgmatrix_2_2015.1[i,11] <- "분류조회"
                if(grepl("상세조회", pgmatrix_2_2015.1[i,11]))
                        pgmatrix_2_2015.1[i,11] <- "상세조회"
        }
        
        pgdataframe_2_2015.1 <- data.frame(pgmatrix_2_2015.1)
        colnames(pgdataframe_2_2015.1) <- c("업무","입찰공고번호","재입찰번호","공고명","수요기관","개찰일시","참가수","낙찰예정자","투찰금액/투찰금리","투찰률(%)","진행상황")
        write.csv(file=file_name, x=pgdataframe_2_2015.1)
        
}
