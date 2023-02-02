
# Welcome to MetaboReport &nbsp;<img src='www/logo.png' align="right" height="200"/>

<b><span style="color:#F17F42">MetaboReport</span></b> provides users a flexible and efficient way to generate a typical metabolomics report.

In particular, it allows to: 

(1). 


(2). 

---

# 1. How does Metaboreport Work?


The use of MetaboReport is pretty simple. You can follow the user guide in each tab to perform your data analysis.

Below is the general workflow of MetaboReport:

<br></br>

<img src='www/workflow.png' alt='workflow' title='workflow' style="vertical-align:middle;margin:0px 100px" width='700'/>

<br></br>


## 2. How to Prepare Data Table for MetaboReport?

MetaboReport accepts a variety of outputs generated from different software tools, such as **XCMS**, **MSDIAL**, or **Compound Discoverer**. In general, the output table can be submitted to MetaboReport with very little modification.

Metaboreport (as well as any other software) requires at least two pieces of information for metabolomics data analysis, i.e. `Peak area` and `sample metadata` information. 

> `Peak Area`: is the area under a peak (peak area count). It is a measure of the concentration of the peak it represents. Sometimes, **peak intensity** or **peak height** maybe used to represent the the concentration of the peak, depending on the software tools used for data processing.  
> `Sample Metadata`: or sample label or group label. It is used to describe the sample. For instance, you want to compare the metabolic profiles between wild type and a mutant. Here **wild type** and **mutant** are sample metadata. Note that one sample can have more than one type of metadata, for instance, if your want to compare the metabolic profiles between male and famale mouse on the same set of sample. Then the sample will have the **Male** and **Female** metadata as well.

---

Metaboreport provides two ways to prepare the data information:

1. The peak area and metadata are in the same table. You only need to upload one single table for data analysis.

2. The peak area table and metadata table are two separated tables. You need to upload both for data analysis.

To better show you how to modify the table, let's start with one example. You want to compare the metabolic profiles between a group of wild type (**WT**) and Mutant (**MU**) mice; meanwhile, you would also like to know if there are any metabolic differences between **Male** and **Female** mice. Pooled QC (**QC**) were used as quality control in your study. Table 1 summarizes the sample information in your study.

---

**Table 1**. Experiment Design of a Metabolomics Study

|Sample  | Sample Label 1 | Sample Label 2 | 
| ------------- | ------------- | ------------- |
| Sample1 | WT | Male|
| Sample2 | WT | Female |
| Sample3 | WT | Male |
| Sample4 | WT | Female |
| sample5 | WT | Male |
| Sample6 | WT | Male|
| Sample7 | MU | Female |
| Sample8 | MU | Male |
| Sample9 | MU | Female |
| sample10 | MU | Male |
| sample11 | MU | Female |
| sample12 | MU | Female | 
| Sample13 | QC | QC |
| sample14 | QC | QC |
| sample15 | QC | QC |
| sample16| QC | QC |

---

After LCMS Analysis and data processing, you got a data table like below:

---

**Table 2**. Sample feature table

mz | Retention Time | Identification | PeakArea of Sample1 | PeakArea of Sample2 | .... | PeakArea of Sample16 |
| ------------- | ------------- | ------------- | ------------- | ------------- | ------------- | ------------- |
| 133.014 | 3.23 | Malic acid| 123000 | 120000 | ... | 801000 |
| 144.012 | 2.44 |  Unknown| 678555 | 666000 | ... | 1200 |
| 333.331 | 10.18| Unknown | 3400 | 2000 | ... | 560000 |
| 234.446 | 5.52 | Unknown | 45000 | 55000 | ... | 46000 |
| ... | ... | ... | ... | ... | ... | ... | ... |
| 1034.552 | 25.33 | Tomatine | 4780 | 4300 | ... | 12000 |


---

### 2.1 Method 1: The peak area and metadata are in the same table.

The method one is simple, you only need to modify the peak area columns shown in Table 2.

For instance, the `PeakArea of Sample1` needs to be modified to `Sample_Sample_WT_Male_1`. See Table 3 for details.

**Explanation**:

> - Keyword `Sample` need to be added in **Peak Areas** columns are so that MetaboReport will know which columns contain peak area information. Pay attention that the keyword `Sample` is case sensitive.
> - Sample1 has two metadata (see Table 1), so each `metadata`, i.e. **WT** and **MALE**, are added after `Sample` keyword, and they are separated by `_`. Note that `MetaData` are also case sensitive, **WT** and **wt** are different. 
> - A `unique number` is added after metadata. This is make each sample unique.
> - all the other columns are not important or necessary for MetaboReport. You don't have to delete them, you can leave them as they are.  

---

**Table 3**. Sample-Feature table for MetaboReport prepared by method 1.
mz | Retention Time | Identification | Sample_WT_Male_1 | Sample_WT_Female_2 | .... | Sample_QC_QC_16 |
| ------------- | ------------- | ------------- | ------------- | ------------- | ------------- | ------------- |
| 133.014 | 3.23 | Malic acid| 123000 | 120000 | ... | 801000 |
| 144.012 | 2.44 |  Unknown| 678555 | 666000 | ... | 1200 |
| 333.331 | 10.18| Unknown | 3400 | 2000 | ... | 560000 |
| 234.446 | 5.52 | Unknown | 45000 | 55000 | ... | 46000 |
| ... | ... | ... | ... | ... | ... | ... | ... |
| 1034.552 | 25.33 | Tomatine | 4780 | 4300 | ... | 12000 |

---

> **Note** that you can also name your raw data in this way before you perform data analysis, then the generated sample-feature table can be directly used by Metaboreport.  


### 2.1 Method 2: The peak area table and metadata table are two separated tables

The second method requires to prepare an additional metadata table.

For **peak area table**:

> - Keyword `Sample` need to be added in **Peak Areas** columns are so that MetaboReport will know which columns contain peak area information. Pay attention that the keyword `Sample` is case sensitive.
> - A `unique number` is needed after the `Sample` Keyword. This is make each sample unique. 
> - all the other columns are not important or necessary for MetaboReport. You don't have to delete them, you can leave them as they are.  
> - The peak area table can be also prepared exactly in the same way as shown in Mehtod 1.

See Table 4 for details.

---

**Table 4**. Peak area table for MetaboReport prepared by method 2.

mz | Retention Time | Identification | Sample1 | Sample2 | .... | Sample16 |
| ------------- | ------------- | ------------- | ------------- | ------------- | ------------- | ------------- |
| 133.014 | 3.23 | Malic acid| 123000 | 120000 | ... | 801000 |
| 144.012 | 2.44 |  Unknown| 678555 | 666000 | ... | 1200 |
| 333.331 | 10.18| Unknown | 3400 | 2000 | ... | 560000 |
| 234.446 | 5.52 | Unknown | 45000 | 55000 | ... | 46000 |
| ... | ... | ... | ... | ... | ... | ... | ... |
| 1034.552 | 25.33 | Tomatine | 4780 | 4300 | ... | 12000 |

---

For **Metadata table**:

> You need to prepare a seperate table to descripe the metadata of each sample. See Table 5. 
> The header names are not important. You can give any names, but the first column must be sample. 
> Note that `MetaData` are case sensitive, so pay attention to the consistancy.

**Table 5**. Metadata table for MetaboReport prepared by method 2.

|Sample  | Sample Label 1 | Sample Label 2 | 
| ------------- | ------------- | ------------- |
| Sample1 | WT | Male|
| Sample2 | WT | Female |
| Sample3 | WT | Male |
| Sample4 | WT | Female |
| sample5 | WT | Male |
| Sample6 | WT | Male|
| Sample7 | MU | Female |
| Sample8 | MU | Male |
| Sample9 | MU | Female |
| sample10 | MU | Male |
| sample11 | MU | Female |
| sample12 | MU | Female | 
| Sample13 | QC | QC |
| sample14 | QC | QC |
| sample15 | QC | QC |
| sample16| QC | QC |

---
> **Notice** that: the sample names in Table 4 and Table 5 don't have to be the same, but the samples in peak area table and metadata table must be in the same order; otherwise, wrong metadata will be assigned to the samples. See Figure 2 below.

**Figure 2**. Pay attention that the samples in peak area table and metadata table must be in the same order.

<img src='www/notice.png' alt='workflow' title='workflow' style="vertical-align:middle;margin:0px 100px" width='700'/>

---

# 3. Video Tutorial

We have also prepared a short video tutorial. Please click [Tutorial Video]() to watch.

---


# 4. Feature Request/Bug Report

Please contact [Dr.Yonghui Dong](mailto:yonghui.dong@gmail.com) to report any bugs or requst new features.

---
<a href= 'https://www.weizmann.ac.il'><img src='www/WIS.png' alt='WIS' title='Weizmann Institute of Science' width='300'/></a>
