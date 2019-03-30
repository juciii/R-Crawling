# 2019-03-10
#####################################
# 네이버 실검 1위-20위 크롤링해오기.
if(!require(rvest)){install.packages(rvest)}:library(rvest)

url = "https://www.naver.com/"
htxt = read_html(url, encoding="utf-8")
# htxt
node = html_nodes(htxt, "span.ah_k") %>% html_text()
# node
node[1:20]

#####################################
# match 관련
pattern = "김성주"
text = "KUCC 회장 김성주라고 합니다."
regexpr(pattern, text)
regmatches(text, regexpr(pattern, text))
text %>% regmatches(regexpr(pattern,.))

pattern = "[A-Z]+" # 정규식
text %>% regmatches(regexpr(pattern, .))

#####################################
# 정규식 관련

#####################################
# paste와 paste0 #paste( ,sep="") == paste0() #즉, 공백을 없애준것.
paste("KU","CC")
paste0("KU","CC")
paste("KUCC","회장",":","김성주")
paste(c("KUCC","회장",":","김성주"))
paste(c("hello","YB","OB"),collapse = "-")
paste(c("hello","YB","OB"),c(1,2,3),collapse = "-",sep = "♥")

# 1st_2nd_3rd_4th_5th_6th_7th_8th_9th 만들기


#####################################
# function만들기
# cat문 : print와 비슷하지만, for문과 function에서 print대신 cat사용.
# 마치 파이썬의 return과 비슷하다고도 볼 수 있다?!
func = function(){
  cat("Hello KUCC")
}
func # 
func()

print_text = function(text){
  cat(text) #cat("text")
}
print_text("Hello KUCC")

space = function(x1,x2){
  y = x1^2 + x2^2
  return(y) #cat(y)
}
space(1,2)
space(x1=c(1,2), x2=c(3,4))
space(c(1,2), c(3,4))

####################################
# return과 cat과 print의 차이

####################################
# rbind와 cbind
# append, concat in python
df1 = data.frame(t1=c(1:4), t2=c(2:5), stringsAsFactors = F)
df2 = data.frame(t1=c(1,2,3,4), t2=c(6,7,8,9), stringsAsFactors = F)
df3 = data.frame(t3=c(3:6), t4=c(4:7), stringsAsFactors = F)
rbind(df1,df2)
cbind(df1,df2)
# rbind(df1,df3)
cbind(df2,df3)

####################################
# query 바꾸기
# query란? 데이터베이스에 정보 요청.
query1 = "동아리"
query = iconv(query1, to = "utf-8", toRaw = T)
query
# query2 = iconv(query1, to = "utf-8", toRaw = F)
# query2

# unlist(query)
query = paste0("%", paste(unlist(query), collapse="%"))
query
query = toupper(query) #대문자로
query

####################################
# encoding 해주는 함수
encoding_trans = function(text){
  paste1 = iconv(text, to = "utf-8", toRaw = T)
  paste = paste0("%", paste(unlist(paste1), collapse = "%"))
  paste = toupper(paste)
  return(paste)
}
encoding_trans("고려대학교")

####################################
if(!require(stringr)){install.packages(stringr)};library(stringr)

# str_split
str_split("KUCC 회장 김성주 입니다.", " ") %>% unlist()

# gsub(pattern, replace, text)
gsub("안녕\\?", "hello", "KUCC 안녕?")

#####################################
# 네이버 블로그 제목 따오기("한국어 검색어")
if(!require(rvest)){install.packages(rvest)}:library(rvest)
query = encoding_trans("고려대학교")
url = paste0("https://search.naver.com/search.naver?where=post&sm=tab_jum&query=",query)
html11 = read_html(url, encoding = "utf-8")
html22 = html11 %>% html_nodes("a.sh_blog_title._sp_each_url._sp_each_title") %>% html_text()
html22

# 네이버 블로그 주소 따오기
query = encoding_trans("고려대학교")
url = paste0("https://search.naver.com/search.naver?where=post&sm=tab_jum&query=",query)
html1 = read_html(url, encoding = "utf-8")
html2 = html1 %>% html_nodes("a.sh_blog_title")
html3 = gsub("<a+.+href=","",html2)
html3

pattern1 = "(https)+.+(logNo=)+[0-9]+"
html4 = html3 %>% regmatches(regexpr(pattern1, .)) #regmatches는 이형태로 쓰자.
html4

# 한줄 한줄씩 교차? ㅇㅎ. 데이터 프레임으로 만들어서 열을 바꾸면 되겠구만.
rbind(html22,html4)

######################################
# 블로그 이름 찾는 함수 만들기
# 빈 데이터 프레임 형성
temp = data.frame(stringsAsFactors = F)
# 함수만들기
blognamefunc = function(q,n){
  query = encoding_trans(q)
  for(i in 1:n){
    url = paste0("https://search.naver.com/search.naver?date_from=&date_option=0&date_to=&dup_remove=1&nso=&post_blogurl=&post_blogurl_without=&query=",query,"&sm=tab_pge&srchby=all&st=sim&where=post&start=",(i-1)*10+1)
    html1 = read_html(url, encoding = "utf-8")
    html2 = html1 %>% html_nodes("a.sh_blog_title") %>% html_text() %>% data.frame() #html_text() %>% data.frame()
    temp = rbind(temp, html2)
    cat(i,"\n")
  } 
  return(temp)
}
tt = blognamefunc("고려대학교", 5)
tt
# csv 저장
setwd("C:/Users/Juciii/Desktop/Rcrawling")
write.csv(tt,"blogname.csv",row.names = F)


#######################################
# 이미지 크롤링
if(!require(RCurl)){install.packages("RCurl");library(RCurl)}
if(!require(XML)){install.packages("XML");library(XML)}
if(!require(dplyr)){install.packages("dplyr");library(dplyr)}
if(!require(rvest)){install.packages("rvest");library(rvest)}

query = "신민아"
query = encoding_trans(query) # 검색어가 영어면 이거 주석처리
url = paste0("https://search.naver.com/search.naver?where=image&sm=tab_jum&query=",query)
html1 = read_html(url, encoding = "utf-8")
html2 = html1 %>% html_nodes("a")

img_pattern = "<img.*?>" # *:적어도 0번 매칭. +:적어도 1번 매칭. ?:많아야 1번 매칭. {n}:정확히 n번 매칭. {n,m}:n번에서 m번 매칭한다.
img_tag = html2 %>% regmatches(regexpr(img_pattern, .))
img_tag

img_add_pat = "(https)+.+(type=b400)"
img_add_tag = img_tag %>% regmatches(regexpr(img_add_pat, ., perl = T))

# 다운로드
getwd()
setwd("C:/Users/Juciii/Desktop/Rcrawling")
for(i in 1:10){
  download.file(img_add_tag[i], paste0("신민아", i, ".jpg"), mode = "wb")
}

########################################
# 이미지 크롤링 함수 만들기
if(!require(RCurl)){install.packages("RCurl");library(RCurl)}
if(!require(XML)){install.packages("XML");library(XML)}
if(!require(dplyr)){install.packages("dplyr");library(dplyr)}
if(!require(rvest)){install.packages("rvest");library(rvest)}

img_download = function(q,n){
  query = encoding_trans("고려대")
  url = paste0("https://search.naver.com/search.naver?where=image&sm=tab_jum&query=",query)
  html1 = read_html(url, encoding = "utf-8")
  html2 = html1 %>% html_nodes("a")
  
  img_pattern = "<img.*?>"
  img_tag = html2 %>%  regmatches(regexpr(img_pattern, .))
  
  img_add_pattern = "(https)+.+(type=b400)"
  img_add_tag = img_tag %>% regmatches(regexpr(img_add_pattern, ., perl = T))
  
  setwd("C:/Users/Juciii/Desktop/Rcrawling")
  for(i in 1:n){
    download.file(img_add_tag[i], paste0(q,i,".jpg"), mode = "wb")
  }
}
img_download("고려대",5)

#########################################
# 구글 주소 찾기 크롤링 #cp949
get_locate = function(q){
  q1 = encoding_tr(q)
  url = paste0("https://www.google.co.kr/search?source=hp&ei=7vaAXK_0HMbqwQOkmZKwBA&q=",q1,"&btnK=Google+%EA%B2%80%EC%83%89&oq=tjdnftlflqeo&gs_l=psy-ab.3.0.0j0i131j0l8.2204.4287..5791...1.0..1.346.1477.11j0j1j1......0....1..gws-wiz.....6..35i39j0i10.fyWjtPU6ylE#btnK=Google%20%EA%B2%80%EC%83%89&spf=1551955702430")
  html = read_html(url,encoding = "cp949")#구글에서는 utf-8보다 cp949가 더 좋아
  html1 = html %>% html_nodes("div") %>% html_nodes("span") %>% html_text()
  p = grep("주소",html1)+1
  return(html1[p])
}
get_locate("고려대")


##########################################
#사람인 크롤링
setwd("C:/Users/Juciii/Desktop/Rcrawling")
library(rvest)

df = data.frame(stringsAsFactors = F)
for(i in 30750:30740){
  #i = 30750
  url = paste0("http://www.saramin.co.kr/zf_user/public-recruit/coverletter?real_seq=", i)
  html = read_html(url, encoding="utf-8")
  
  tmp = html %>% html_nodes("div.cont")
  question = tmp %>% html_nodes("ol.list") %>% html_text()
  category = tmp %>% html_nodes("span.tag_apply") %>% html_text()
  temp = tmp %>% html_nodes("div.box_ty3") %>% html_text()
  
  answer = gsub("(접기)|[[:cntrl:]]|[[:punct:]]|(글자수 )+[0-9]+(자)+|[0-9]+(Byte)+",
                "",temp)
  #접기에 앞뒤로 괄호를 하지 않으면 접과 기가 다 사라진다. cntrl과 punct는 특수문자 없애기
  #텍스트마이닝에서 정규방정식을 알면 필요없는 부분 없애기 편하다
  #(글자수 )+[0-9]+(자) 이 형식 없애기.
  answer = paste(answer,collapse = " ")
  if(answer != ""){
    df_tmp = data.frame(Q = question,
                        C = category,
                        A = answer,
                        stringsAsFactors = F)
  }#숫자화 되는 경우 stringsAsFactors는 그냥 필수적으로 써.
  df = rbind(df,df_tmp)
  cat("i=",i,"\n")
}

write.csv(df,"temp1.csv",row.names = F)

















