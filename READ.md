과학적 주식투자를 위한 선행연구
=======================
>ITWILL 빅데이터분석 플랫폼 Semi Progject
>-----------------------------------
* CSV 파일 In memroy
>> [CSV 파일 병합](https://github.com/DominKim/Domin/blob/master/ITWILL_Semiproject/Join_rawdata.R)

~~~r
# raw_data 병합
if(!require("readxl"))install.packages("readxl");library(readxl)
library(xlsx)
library(data.table) # rbindlist
if(!require("stringr"))install.packages("stringr");library(stringr)
~~~
* 데이터 전처리
>> [데이터 전처리](https://github.com/DominKim/Domin/blob/master/ITWILL_Semiproject/data_preprocessing.R)
*
*
* Random Forest 결과
>> [R.F 결과]<img src="./ITWILL_Semiproject/output/랜덤포레스트결과값 상관도.png" width="80%" height="50%"></img>
