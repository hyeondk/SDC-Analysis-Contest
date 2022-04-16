install.packages('xlsx')
install.packages("rJava")
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_211")
library(xlsx)

### 출처 : KB주택가격동향_시계열

# 서울 주택매매지수(종합 = 아파트+단독+연립)
seoul <- read.xlsx('서울.xlsx',1, encoding = 'UTF-8', stringsAsFactors = F)
names(seoul) <- c("연도", month.abb)
write.xlsx(seoul, file = 'seoul.xlsx')

# 서울 주택매매지수(아파트)
seoul_A <- read.xlsx('서울.xlsx', 2, encoding = 'UTF-8', stringsAsFactors = F)
names(seoul_A) <- c("연도", month.abb)
write.xlsx(seoul_A, file = 'seoul_A.xlsx')
