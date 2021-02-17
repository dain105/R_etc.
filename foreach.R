library(foreach)
library(doSNOW)

# 클러스터 갯수
worker=makeCluster(2)
registerDoSNOW(worker)

#for 루프 대신에 foreach 함수를 사용한다.
# foreach() 함수 다음에 위치한 %dopar% 연산자가 다수 코어에 독립적으로 foreach 연산
# 즉, foreach 루프 몸통부문을 실행하도록 지시한다.
#systemp.time으로 시간을 비교해본다.
#foreach 문이 더 빠르게 돌아가는 것을 알 수 있다.
system.time(x <- foreach(i=1:4) %dopar%  rnorm(5000*1000))
system.time(x <- for (i in 1:4) {rnorm(5000*1000)})


# 클러스터 중지
stopCluster(worker)

