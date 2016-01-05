############### FAVOR EXCHANGE (Ishii, 2009) ##########################
compareNA <- function(v1,v2) {
        same <- (v1 == v2) | (is.na(v1) & is.na(v2))
        same[is.na(same)] <- FALSE
        return(same)
}

numfirms <- length(unique(bidResult_tka_2010to2014$상호명[compareNA(bidResult_tka_2010to2014$순위, 1)]))

#score matrix 생성
score_matrix <- matrix(0, nrow=numfirms, ncol=nrow(WbidderList_tka_2010to2014))
rownames(score_matrix) <- unique(WbidderList_tka_2010to2014$상호명)
colnames(score_matrix) <- 1:nrow(WbidderList_tka_2010to2014)
score_matrix <- as.data.frame(score_matrix)

#공고 시간의 흐름을 반영하는 시간 변수 t 생성
WbidderList_tka_2010to2014$t <- 1:nrow(WbidderList_tka_2010to2014)

#balance matrix 생성
balance_matrix <- matrix(0, nrow=numfirms, ncol=numfirms)
rownames(balance_matrix) <- unique(WbidderList_tka_2010to2014$상호명)
colnames(balance_matrix) <- unique(WbidderList_tka_2010to2014$상호명)
balance_matrix <- as.data.frame(balance_matrix)

#backlog matrix 생성
backlog_matrix <- matrix(NA, nrow=numfirms, ncol=nrow(WbidderList_tka_2010to2014))
rownames(backlog_matrix) <- unique(WbidderList_tka_2010to2014$상호명)
colnames(backlog_matrix) <- 1:nrow(WbidderList_tka_2010to2014)
backlog_matrix <- as.data.frame(backlog_matrix)

#win matrix 생성
win_matrix <- matrix(NA, nrow=numfirms, ncol=nrow(WbidderList_tka_2010to2014))
rownames(win_matrix) <- unique(WbidderList_tka_2010to2014$상호명)
colnames(win_matrix) <- 1:nrow(WbidderList_tka_2010to2014)
win_matrix <- as.data.frame(win_matrix)

#####반복####
for(iter in 1:nrow(WbidderList_tka_2010to2014)){
        #입찰공고번호
        bidno <- as.character(WbidderList_tka_2010to2014$입찰공고번호[iter])
        
        #승자 파악
        winner <- WbidderList_tka_2010to2014$상호명[iter]
        
        #ring member 파악
        bidding_firms <- bidResult_tka_2010to2014$상호명[bidResult_tka_2010to2014$입찰공고번호 == bidno]
        ring_member <- bidding_firms[bidding_firms %in% cartel]
        
        #cluster influence
        #clusterinfluence_matrix[ring_member,iter] <- length(ring_member) / sum(ring_index)
        
        #winner 삭제
        ring_member <- ring_member[ring_member != winner]
        
        #ring member가 자신을 제외하고 1명이상 있을 경우만 의미있음
        if(length(ring_member) > 0){
                
                #score 계산
                #t+1시점의 score는 t 시점까지의 balance로 계산되는 것이므로 balance 보다 먼저 계산해야 함
                score_matrix[winner,iter] <- sum(balance_matrix[winner, ring_member] > 0)
                for(j in 1:length(ring_member)){
                        score_matrix[ring_member[j], iter] <- sum(balance_matrix[ring_member[j], ring_member] > 0) 
                }
                
                #favor 계산
                favor <- WbidderList_tka_2010to2014$입찰금액.원.[iter] / length(ring_member) #단위: 억 원
                
                #balance 갱신
                balance_matrix[winner, ring_member] <- balance_matrix[winner, ring_member] - favor
                balance_matrix[ring_member, winner] <- balance_matrix[ring_member, winner] + favor
                
                #win 갱신
                win_matrix[winner,iter] <- 1
                win_matrix[ring_member, iter] <- 0
                
                #backlog 갱신                 
                #t+1 경매에서는 t 시점까지의 계약금액 총합을 의미함                 
                if(length(which(WbidderList_2010to2014$입찰공고번호 == bidno)) > 0){
                        findBacklog <- function(x){
                                ring_index <- WbidderList_2010to2014$상호명_1[1:which(WbidderList_2010to2014$입찰공고번호 == bidno)-1] == x
                                if(sum(ring_index, na.rm=T) > 0){
                                        sum(WbidderList_2010to2014$낙찰금액.원.[1:which(WbidderList_2010to2014$입찰공고번호 == bidno)-1][ring_index], na.rm=T)
                                }else{
                                        0       
                                }
                        }
                        backlog_matrix[ ,iter] <- sapply(cartel, findBacklog)
                }
                
        }
        print(iter)
}
