library(dplyr)
library(readr)
library(stringr)

# 加载数据
scientist <- read_csv("scientist.csv")
inst_wos_dict <- read_csv("inst_wos_dict.csv")
cddt_paper <- read_csv("cddt_paper.csv")
cite_all <- read_csv("cite.csv")

# 预处理数据
# 预处理数据：已完成，不需要重命名操作
scientist <- scientist %>% mutate(inst = tolower(inst))
inst_wos_dict <- inst_wos_dict %>% mutate(inst = tolower(inst), wos = tolower(wos))
cddt_paper <- cddt_paper %>% mutate(addr = tolower(addr), type = as.numeric(type))

# 标准化机构名称
inst_map <- function(cv, inst_dict, db = "wos") {
  # 根据不同的数据库处理机构名称
  # 当前示例只针对"wos"，可根据需要扩展到其它数据库
  if(db == "wos") {
    cv <- cv %>%
      inner_join(inst_dict, by = "inst") %>%
      # 假设inst_dict中的wos列就是我们需要的标准化机构名称
      select(inst = wos, startyear, endyear) %>%
      distinct()
  } else {
    print(paste("Database", db, "not supported."))
  }
  
  # 确保返回的数据框列名一致
  names(cv) <- c("inst", "startyear", "endyear")
  return(cv)
}


# 筛选符合条件的论文
cv_filter <- function(paper, cv, year_lag = 2) {
  paper %>%
    filter(pub_year >= cv$startyear & pub_year <= cv$endyear + year_lag, type == 1) %>%
    select(ut_char) %>%
    distinct() %>%
    pull(ut_char)
}

# 扩展论文集基于引用关系
cite_glue <- function(pid, cite) {
  cited_pids <- cite %>% filter(citing_ut %in% pid) %>% pull(cited_ut) %>% unique()
  citing_pids <- cite %>% filter(cited_ut %in% pid) %>% pull(citing_ut) %>% unique()
  new_pids <- unique(c(cited_pids, citing_pids))
  setdiff(new_pids, pid) # 返回新增的PID，避免重复
}

# 实施姓名消岐算法
cv_disam <- function(paper, cv, year_lag1, year_lag2, cite) {
  # 筛选使用全名的论文
  pid_stage1 <- cv_filter(paper, cv, year_lag1)
  
  # 扩展论文集
  pid_core <- pid_stage1
  repeat {
    pid_add <- cite_glue(pid_core, cite)
    if (length(pid_add) == 0) break
    pid_core <- unique(c(pid_core, pid_add))
  }
  
  # 筛选新添加的论文
  paper_stage2 <- paper %>% filter(ut_char %in% pid_core, type == 2)
  pid_stage2 <- cv_filter(paper_stage2, cv, year_lag2)
  
  unique(c(pid_stage1, pid_stage2))
}

# 执行姓名消歧算法
result <- data.frame(uniqueID = character(), pid = character(), stringsAsFactors = FALSE)
for(id in unique(scientist$uniqueID)){
  cv <- scientist %>% filter(uniqueID == id)
  cv_mapped <- inst_map(cv, inst_wos_dict, db = "wos")
  
  paper <- cddt_paper %>%
    filter(uniqueID == id) %>%
    mutate(initials = as.numeric(type == 2), pub_year = as.numeric(pub_year)) %>%
    rename(pid = ut_char, aff = addr) %>%
    select(pid, aff, pub_year, initials)
  
  pid_disam <- cv_disam(paper, cv_mapped, 2, 2, cite_all)
  result <- rbind(result, data.frame(uniqueID = id, pid = pid_disam, stringsAsFactors = FALSE))
}

# 输出或保存结果
print(result)
# write.csv(result, "disambiguated_papers.csv", row.names = FALSE)
