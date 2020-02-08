library(tidyverse)
library(rlist)
rm(list=ls())

## ------------------------------------
## 各サイトの基本情報を読み込む
## ------------------------------------
aiueo <- read.csv("../Files/PlotInfo/PlotInfo.csv")
ringo <- aiueo[,c(3,4,5,13,14,15,16)] #必要な場所のみ取り出す
colnames(ringo) <- c( # 日本語の列名から英語に変更
  "SiteID", ## 各サイトのID
  "PlotID", ## 各調査プロットのID
  "ForestType", ## 森林タイプ
  "Longtitude", ## 調査プロットの経度
  "Latitude", ## 調査プロットの緯度
  "Elevation", ## 調査プロットの標高
  "AveTem" ## 調査プロットの平均気温
)

## ----------------------
## 毎木調査のデータの読み込み、Tidyにする過程
## ----------------------
files <- list.files(path = "../Files/AllTree",full.names = T)
l <- lapply(files,function(x){ #ファイルを読み込み、リストにしまう
    file <- read.csv(x,comment.char="#")
    file$PlotID <- rep(x)
    return(file)
    }
)

pole <- l #変数 lはあまりいじりたくない
pole[[35]] <- l[[35]] %>% # 謎の全角英語の列名がある場所を半角英語に変換
    rename(
        stem_xcord= stem_ｘcord,
        stem_ycord= stem_ｙcord
    )


john <- pole %>%
    list.stack(fill=T) %>%
    select(
        starts_with("gbh"),
        starts_with("dl"),
        starts_with("error"),
        spc_japan,
        matches(".cord$"),
        PlotID,
        NULL
    ) %>%
    mutate(
        PlotID= substr(PlotID,18,23) %>% as.factor(),
        ID= row_number(),
        NULL
    )  %>%
    pivot_longer(
        -c(ID,spc_japan,matches("cord"),PlotID),
        names_to=c(".value","year"),
        names_pattern="(.*)(..)"
    ) %>%
    mutate( #s_dateのデータに入力ミスが目立つ、年単位の計算だと「**09」の方の上方を使ったほうが正確性が高い
        year= ifelse(
            year>50,
            as.numeric(year)+1900, #1999年のデータもあるため、
            as.numeric(year)+2000
            )
    )

harrison <- john %>%
    left_join(ringo,by="PlotID")
