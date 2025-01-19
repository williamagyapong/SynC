library(readr)

# If the whole repo folder/directory is opened as an R project, current working dir should point
# to "SynC". Confirm by running getwd() on this file
file_dir = 'models/'
data_dir = 'data/'
source(paste(file_dir, 'functions.R', sep = ''))
set.seed(123)
postal = 'V3N1P5' # Change this to any other postal code

#####Generating Age, Gender and Ethnicity#########
pp = read_csv(paste(data_dir, "PP.csv", sep = ''))
pp_sub = pp[pp$PostCode == postal, ]

### Selecting desired columns 
# Numbers must correspond to column indices from the corresponding aggregated data
age_groups = c(32:34, 36:45, 47:50, 52:55, 60:62, 64:73, 75:78, 80:83)
gender_groups = c()
ethno_groups = c(3, 43, 44, 52, 61, 66, 77, 84, 100, 119, 125, 146, 172, 241, 270, 288, 
                 289, 291, 292, 294, 296, 299, 302, 306)
relig_groups = c(3,4,14:20)
ed_groups = c(3:4, 6, 9, 12, 15, 16)

# groups = list(age_groups, gender_groups, ethno_groups, relig_groups, ed_groups)
groups = list(age_groups, ethno_groups, relig_groups, ed_groups)

# age_names = c('PP_AVG')
gender_names = c('PP_MALE')
ethno_names = c("ET_ABO", "ET_AME", "ET_CAN", "ET_BRIO", "ET_FREO", "ET_WEUO", "ET_NEUO", 
                "ET_EEUO", "ET_SEUO", "ET_OEUO", "ET_CARO", "ET_LAMO", "ET_AFRO", "ET_WASIAO", 
                "ET_SA",  "ET_CHIN", "ET_FIL", "ET_INDO", "ET_JAP", "ET_KOR", "ET_MALAY",
                "ET_TAIW", "ET_VTN", "ET_OCEO")
relig_names = c("RL_BUD", "RL_CHRI", "RL_HIND", "RL_JEW", "RL_MUSL", "RL_SIKH", "RL_ABOR", 
                "RL_OTRL", "RL_NON")
ed_names = c("ED_15NC", "ED_15HSC", "ED_15TRC", "ED_15COL", "ED_15BD", "ED_15MAS", "ED_15DOC")


# age_sample_prob = pp_sub[, age_gender_groups]/sum(pp_sub[, age_gender_groups])
age_sample_prob = pp_sub[, age_groups]/sum(pp_sub[, age_groups])
age_names = names(age_sample_prob)
# names = list(age_names, gender_groups, ethno_names, relig_names, ed_names)
names = list(age_names, ethno_names, relig_names, ed_names)

#### Sample individuals
individual = data.frame(sample(names[[1]], pp_sub$PP_TOT, replace = TRUE, prob = age_sample_prob))
colnames(individual) = 'Demographics'

# core = pp[, age_gender_groups]/pp$PP_TOT
core = pp[, age_groups]/pp$PP_TOT
write_csv(core, paste(data_dir, 'core.csv', sep = ''))
write_csv(individual, paste(data_dir, 'individual.csv', sep = ''))

##########Generate Ethnicity
process_file(file_path = paste(data_dir, 'ET.csv', sep = ''), var_name = 'Ethnicity', postal = postal, 
             index = groups[[2]])

##########Generate Religion
process_file(file_path = paste(data_dir, 'RL.csv', sep = ''), var_name = 'Religion', postal = postal, 
             index = groups[[3]])

##########Generate Education
process_file(file_path = paste(data_dir, 'ED.csv', sep = ''), var_name = 'Education', postal = postal, 
             index = groups[[4]])
