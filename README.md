# 如何构建该网站


## 数据准备

构建拟南芥的基因名和别名的对应关系(数据来源: <https://zenodo.org/record/2530282>)

```R
alias_df <- fread("data/gene_aliases_20171231.txt")
saveRDS(alias_df, "data/alias_data_frame.rds")
```

构建基因ID列表

```bash
grep '[[:blank:]]gene[[:blank:]]' Araport11_GFF3_genes_transposons.gff   | cut -f 9 | cut -d ';' -f 1 | cut -d '=' -f 2 | grep -v '\.' | uniq | sort  -V > gene_list.txt
```

之后导入R语言，保存为Rdata

```bash
gene_df <- readLines("data/gene_list.txt")
saveRDS(gene_df, "data/gene_list.rds")
```


