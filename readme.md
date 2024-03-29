[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-24ddc0f5d75046c5622901739e7c5dd533143b0c8e959d652212380cedb1ea36.svg)](https://classroom.github.com/a/boRGmXHL)
科技论文作者姓名消岐，即通过作者的基本信息得到其准确发表记录，的问题科学经济学研究中的基础问题。只有解决这一问题才可能将研究导入个体层面。

现在有20名科学家，我们的目标是获取这些科学家的论文信息。

我们首先收集到了他们工作的履历信息（储存在scietist.csv），然后我们通过科学家的全名以及科学家的名字缩写从web of science数据库中检索出来所有对应名字所发表的论文以及作者的地址，最后我们下载了所有这些论文的参考文献与引用这些论文的文献。

我们将利用上述信息获得科学家论文的清单。

计算步骤如下，

第一、筛选出发表地址、论文发表时间与科学家履历匹配、且论文名字使用了全名的（type==1）的论文；

第二、通过论文间的引用关系将第一步中论文的引用以及被引关系的论文，注意新添加的论文会引入新的引用关系，重复本步骤，直到没有新论文可以被添加。

第三、筛选新添加的论文，使其满足发表地址、论文发表时间与科学家履历匹配。

路径data/assignment5_cvand数据

scientist.csv

inst_wos_dict.csv

cddt_paper.csv

cite.csv

数据变量说明

|变量|	说明|
|-----|-----|
|uniqueID	|科学家ID|
|inst|科学家工作机构|
|startyear	|开始工作年份|
|endyear|结束工作年份|
|ut_char|	论文ID|
|addr|	论文地址|
|item_type|	论文类型|
|pub_year|	论文发表年份|
|type	|论文对应的作者名称类型，1表示全名，2表示缩写|
|wos	|研究机构在wos数据库中对应的地址|
|citing_ut|	施引论文ID|
|cited_ut	|被引论文ID|

更多算法请参考：

1. 刘玮辰,史冬波,李江.基于职业经历和引文网络的华人姓名消歧算法[J].信息资源管理学报,2020,10(06):82-89+100.DOI:10.13365/j.jirm.2020.06.082

2. Dongbo Shi et al. ,Has China’s Young Thousand Talents program been successful in recruiting and nurturing top-caliber scientists?.Science379,62-65(2023).DOI:10.1126/science.abq1218 (附件)

**通过R语言实现上述算法**
