# 실시간 검색어 크롤링
if(!require(rvest)){install.packages("rvest");library(rvest)}
# rvest가 크롤링을 위한 라이브러리
# require는 TRUE, FALSE를 리턴해.
# 있으면 그대로, 없으면 설치 => 누군가에게 배포할 때 코드.

url = "https://www.naver.com/"
htxt = read_html(url,encoding="utf-8")
htxt
t1 = html_nodes(htxt, 'span.ah_k') %>% html_text()
t1
t1[1:20]

#논리방정식 

if(1!=1){
  print("hello")
}
if(1!=1){print("hello")}

#match
pattern = "김혜림"
txt = "안녕하세요 tave 회장 김혜림입니다."
regexpr(pattern,txt)
# pattern이 txt에서 15번째에서 3개가 매치된다.
regmatches(txt,regexpr(pattern,txt))
# 그 15번째에서 3개를 가져온다.
txt %>% regmatches(regexpr(pattern, .))
# %>% txt로 다음의 함수를 실행 하겠다.

pattern = "[a-z]+"
txt %>% regmatches(regexpr(pattern, .))

#paste
paste("hi","TAVE")
paste("hi","TAVE",sep = "♥")
paste("hi","TAVE",sep = "")
paste0("hi","TAVE")
paste(c("hello","YB","OB"),collapse = "-")
paste(c("hello","YB","OB"),c(1,2,3),collapse = "-",sep = "♥")

#function 만들기
cat("hello") #print문과 동일하지만 for문,function에서 print 대체
helloTAVE = function(){
  cat("hello tave")
}
helloTAVE()

print_text = function(text){
  cat(text)
}
print_text("hi tave")

func = function(a,b){
  c = a + b -a^2
  return(c)
}
func(3,1) #3 + 1 - 3^2 = -5

#rbind/cbind
d1 = data.frame(t1=c(1:4),t2=c(5:8),stringsAsFactors = F)
d2 = data.frame(t1=c(5:7),t2=c(9:11),stringsAsFactors = F)
d3 = data.frame(t3=c(5:8),t4=c(9:12),stringsAsFactors = F)
rbind(d1,d2)
cbind(d1,d2)
cbind(d1,d3)

#query 바꾸기
query1 = "tave 동아리" #빅데이터 동아리
query = iconv(query1, to = 'UTF-8', toRaw = T)
# iconv(query, to = "UTF-8", toRaw = F)화
# iconv?
query = paste0('%', paste(unlist(query), collapse = '%'))
query
query = toupper(query) # 대문자호
query


# encoding 변화
encoding_tr = function(query){
  query = iconv(query, to = 'UTF-8', toRaw = T)
  query = paste0('%', paste(unlist(query), collapse = '%'))
  query = toupper(query)
  return(query)   
}
encoding_tr("tave 동아리")
#####################################

if(!require(stringr)){install.packages("stringr");library(stringr)}
#str_split
str_split("hello hi my name is haley"," ")# %>% unlist() # 앞에 #을 지워서 리스트 형태를 벡터형태로 바꿔줘야해

# gsub(pattern, replace, text) => 
gsub("안녕\\?","hello","tave 안녕?")해
# 정규방정식에서 ?는 한개이상이라는 의미로 쓰기 위해서 앞에 역슬래쉬

#정규방정식 기본 <블로그 참고>
if(!require(rvest)){install.packages("rvest");library(rvest)}
query = encoding_tr("tave 동아리")
url = paste0("https://search.naver.com/search.naver?date_from=&date_option=0&date_to=&dup_remove=1&nso=&post_blogurl=&post_blogurl_without=&query=",query,"&sm=tab_pge&srchby=all&st=sim&where=post&start=1")
html = read_html(url,encoding = "utf-8") #euc-kr,cp949,utf-8
html
html1 = html %>% html_nodes("dt") %>% html_nodes("a") %>% html_text()
html1

html1 = html %>% html_nodes("a.sh_blog_title")
html1
html2 = gsub("<a+.+(href=)+","",html1)
html2
tag_pattern = "(https)+.+(logNo=)+[0-9]+"
html3 = html2 %>% regmatches(regexpr(tag_pattern, .))
html3
blogad = gsub("\\?Redirect=Log&amp;logNo=","/",html3)
blogad


#########################################start################################
tmp = data.frame(stringsAsFactors=F)
# 빈프레임 만들기

for(i in 1:7){
  query = encoding_tr("tave 동아리")
  url = paste0("https://search.naver.com/search.naver?date_from=&date_option=0&date_to=&dup_remove=1&nso=&post_blogurl=&post_blogurl_without=&query=",query,"&sm=tab_pge&srchby=all&st=sim&where=post&start=",(i-1)*10+1)
  # 뒤에 (i-1)*10 +1 은 갯수를 제한 해둔 것. 페이지?
  html = read_html(url,encoding = "utf-8")
  html1 = html %>% html_nodes("dt") %>% html_nodes("a") %>% html_text() %>% data.frame()
  tmp = rbind(tmp,html1)
  cat(i,"\n") # debugging 용도로, 잘되고있는지 확인하려고
}

title_cw_haley = function(q,n){
  tmp = data.frame(stringsAsFactors=F)
  for(i in 1:n){
    query = encoding_trans(q)
    url = paste0("https://search.naver.com/search.naver?date_from=&date_option=0&date_to=&dup_remove=1&nso=&post_blogurl=&post_blogurl_without=&query=",query,"&sm=tab_pge&srchby=all&st=sim&where=post&start=",(i-1)*10+1)
    html = read_html(url,encoding = "utf-8")
    html1 = html %>% html_nodes("dt") %>% html_nodes("a") %>% html_text() %>% data.frame()
    tmp = rbind(tmp,html1)
    cat(i,"\n")
  }
  return(tmp)
}
title_cw_haley("태국여행",10)

setwd("C:/Users/USER/Desktop/haley/TAVE/3기")
write.table(f,"태국여행.txt",row.names = F) #row.names=F는 1,2,3,4,5 행번호 붙는거 없애기.

#이미지 크롤링
if(!require(RCurl)){install.packages("RCurl");library(RCurl)}
if(!require(XML)){install.packages("XML");library(XML)}
if(!require(dplyr)){install.packages("dplyr");library(dplyr)}
if(!require(rvest)){install.packages("rvest");library(rvest)}


url = "https://search.naver.com/search.naver?where=image&sm=tab_jum&query=tave"
#q = "tave"
#url = paste0("https://search.naver.com/search.naver?where=image&sm=tab_jum&query=",q)
#한글이면 iconv해줘야.
htxt = read_html(url)
t1 = html_nodes(htxt, 'a')

img_tag_pattern = "<img.*?>" #img고 뒤에 .은 모든것.
img_tag = t1 %>% regmatches(regexpr(img_tag_pattern, .))

src_href_pattern = "(https:.+type=b400)"
src_href = img_tag %>% regmatches(regexpr(src_href_pattern, ., perl=T))
src_href
# 
getwd()
setwd("C:/Users/USER/Desktop/haley/TAVE/3기/크롤링강의")
download.file(src_href[2], "test_image2.jpg",mode="wb")
# 이거는 다운로드.

for(i in 1:length(src_href)){
  download.file(src_href[i], paste0("image_", i,".jpg"),mode="wb")
}


setwd("C:/Users/USER/Desktop/haley/TAVE/3기/크롤링강의")
image_cw_haley = function(q,n){
  q1 = encoding_tr(q)
  url = paste0("https://search.naver.com/search.naver?where=image&sm=tab_jum&query=",q1)
  htxt = read_html(url)
  t1 = html_nodes(htxt, 'a')
  
  img_tag_pattern = "<img.*?>"
  img_tag = t1 %>% regmatches(regexpr(img_tag_pattern, .))
  
  src_href_pattern = "(https:.+type=b400)"
  src_href = img_tag %>% regmatches(regexpr(src_href_pattern, ., perl=T))
  
  n = length(src_href)
  if(n > 50){
    n = 50
  }
  for(i in 1:n){
    download.file(src_href[i], paste0(q,"image_", i, ".jpg"),mode="Wb")
  }
}
image_cw_haley("신민아",10)
  

################주소크롤링#####################
get_locate = function(q){
  q1 = encoding_tr(q)
  url = paste0("https://www.google.co.kr/search?source=hp&ei=7vaAXK_0HMbqwQOkmZKwBA&q=",q1,"&btnK=Google+%EA%B2%80%EC%83%89&oq=tjdnftlflqeo&gs_l=psy-ab.3.0.0j0i131j0l8.2204.4287..5791...1.0..1.346.1477.11j0j1j1......0....1..gws-wiz.....6..35i39j0i10.fyWjtPU6ylE#btnK=Google%20%EA%B2%80%EC%83%89&spf=1551955702430")
  html = read_html(url,encoding = "cp949")#구글에서는 utf-8보다 cp949가 더 좋아
  html1 = html %>% html_nodes("div") %>% html_nodes("span") %>% html_text()
  p = grep("주소",html1)+1
  return(html1[p])
}
get_locate("고려대")

#####################사람인 크롤링
setwd("C:/Users/Juciii/Desktop/TAVE/크롤링특강")
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
