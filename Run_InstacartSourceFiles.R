###########################   License Short Notice Start  #########################
#     Copyright(C) Michael A Garcia 2017. All Rights Reserved.                    #
#     Contact Author: mgar_datascience@protonmail.com for Licensing Inquries      #
#                                                                                 #
#     This program is distributed in the hope that it will be useful,             #
#     but WITHOUT ANY WARRANTY; without even the implied warranty of              #
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               #
#     GNU General Public License Version 3 for more details.                      #
#     See http://www.gnu.org/licenses for full license.                           #              
#                                                                                 #
###########################   License Short Notice End   #########################
#Sys.setenv(R_MAX_MEM_SIZE=32768)
#Sys.setenv(R_MAX_MEM_SIZE=18432)
#Sys.setenv(R_MAX_MEM_SIZE=16384)
#Sys.setenv(R_MAX_MEM_SIZE=12288)
#Sys.setenv(R_MAX_MEM_SIZE=8192)
#Sys.setenv(R_MAX_MEM_SIZE=10240)
#Sys.setenv(R_MAX_MEM_SIZE=4096)
#Sys.setenv(R_MAX_MEM_SIZE=2048)
#Sys.setenv(R_MAX_MEM_SIZE=1024)
#--max-mem-size = 

#options(repos=structure(c(CRAN="https://cloud.r-project.org/")))
#options(repos=structure(c(CRAN="https://mirrors.nics.utk.edu/cran/")))

# domc is not compatible with 3.4.1
#library(doMC)
#registerDoMC(cores = 8)

source(file.path(getwd(),paste("datasetPrep",".R",sep = "")))
source(file.path(getwd(),paste("datasetPrep_part2",".R",sep = "")))
source(file.path(getwd(),paste("datasetPrep_part3",".R",sep = "")))
source(file.path(getwd(),paste("orders_users_analysis",".R",sep = "")))
source(file.path(getwd(),paste("orders_users_analysis_pt2",".R",sep = "")))
source(file.path(getwd(),paste("orders_users_analysis_pt3",".R",sep = "")))
source(file.path(getwd(),paste("products_aisles_departments_analysis",".R",sep = "")))
source(file.path(getwd(),paste("product_aisles_depts_p2",".R",sep = "")))
source(file.path(getwd(),paste("topproducts",".R",sep = "")))
source(file.path(getwd(),paste("reorders",".R",sep = "")))
source(file.path(getwd(),paste("reordersPt2",".R",sep = "")))
#source(file.path(getwd(),paste("clusterFunction",".R",sep = "")))





