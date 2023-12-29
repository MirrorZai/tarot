library(shiny)

library(here)

jing <- fluidPage( #创建web程序页面
  textInput("num_cards", "请输入要抽取的牌的数量："), #文本输入框，num_cards是id,后面是提示信息
  actionButton("draw_button", "抽牌"),#创建按钮，前面是id，后面是提示内容
  verbatimTextOutput("result")#显示结果，result是id，后面是提示内容
)

file <- here("tarot_interpretation", "tarots interpretations.txt") # 引入文件
interpretations <- read.table(file, header = FALSE, stringsAsFactors = FALSE)
colnames(interpretations) <- c("tarot", "interpretation")

# 定义服务器逻辑
server <- function(input, output) {
  tarot_jing <- data.frame(
    tarot = c( 
      "愚人", "魔术师", "女祭司", "皇后", "皇帝", "教皇", "恋人", "战车", "力量", "隐士", "命运之轮", "正义", "倒吊人", "死神", "节制", "恶魔", "高塔", "星星", "月亮", "太阳", "审判", "世界",
      "权杖王牌", "权二", "权杖三", "权杖四", "权杖五", "权杖六", "权杖七", "权杖八", "权杖九", "权杖十",
      "权杖侍卫", "权杖骑士", "权杖皇后", "权杖国王",
      "宝剑王牌", "宝剑二", "宝剑三", "宝剑四", "宝剑五", "宝剑六", "宝剑七", "宝剑八", "宝剑九", "宝剑十",
      "宝剑侍卫", "宝剑骑士", "宝剑皇后", "宝剑国王",
      "圣杯王牌", "圣杯二", "圣杯三", "圣杯四", "圣杯五", "圣杯六", "圣杯七", "圣杯八", "圣杯九", "圣杯十",
      "圣杯侍卫", "圣杯骑士", "圣杯皇后", "圣杯国王",
      "钱币王牌", "钱币二", "钱币三", "钱币四", "钱币五", "钱币六", "钱币七", "钱币八", "钱币九", "钱币十",
      "钱币侍卫", "钱币骑士", "钱币皇后", "钱币国王"),#创建塔罗矩阵
    orientation = sample(c("正位","逆位"),78,replace=TRUE)#生成78个正位与逆位
  )
  
  #抽牌数量函数
  draw_cards <- function(num_cards){
    drawn <- sample(tarot_jing$tarot, num_cards, replace = TRUE) #随机抽取相应数量牌
    
    result <- lapply(drawn,function(tarot){ #列表操作，drawn是向量，card是其中每个元素，对每个card执行后面的函数
      orientation <- tarot_jing$orientation[tarot_jing$tarot == tarot] #根据牌名获得相应的正逆位
      interpretation <- interpretations$interpretation[tarot_jing$tarot == tarot]
      paste(tarot,":", orientation,", 解释：",interpretation)#paste合并为一个字符串
    })
    
    return(result)
  }
  
  # 定义一个reactive来处理抽牌逻辑
  drawn_cards <- eventReactive(input$draw_button, {#eventReactive函数，input定以输入值，$表示访问具体值，点击draw_button执行代码块
    num <- as.integer(input$num_cards)#把输入的值数值化
    if (!is.na(num) && num > 0){
      draw_cards(num)
    }else{
      return(NULL)
    }
  })
  
  # 输出抽牌结果
  output$result <- renderPrint({ #renderPrint渲染，将结果渲染到output区域
    drawn_cards()
  })
}

# 运行应用
shinyApp(ui = jing, server = server)#ui界面为jing，server服务器逻辑