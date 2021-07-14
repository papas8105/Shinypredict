#!/usr/bin/env python
# coding: utf-8

# ## TRAIN TEST SPLIT function

# This workbook will contain the function used to split any dataset into Train and Test sets

# Function requirements:
# 

# - Input any data set as csv. file
# - Select which portion will be Train and Test data (usually Train 80% Teest 20%)
# - Split initial data set based on that proportion 
# - Plot data set with Train and Test portions on different colours
# - Output results as .csv including new column that identifies both sets

# ## 1 Load required libraries

# In[228]:


import pandas as pd


# In[229]:


import numpy as np


# In[230]:


import matplotlib.pyplot as plt


# In[231]:


import math


# In[232]:


import os


# ## 2 Load input data

# Check which is our working directory

# In[233]:


path = os.getcwd()


# In[234]:


path


# Change current working directory where data is stored

# In[235]:


os.chdir('C:/Pablo UK/46 DATA SCIENCE all/44 Python')


# In[236]:


new_path =  os.chdir('C:/Pablo UK/46 DATA SCIENCE all/44 Python')


# In[237]:


new_path


# In[238]:


os.getcwd()


# Input data

# In[239]:


Sales  = pd.read_csv('Sales_clean_plot_csv.csv') 


# In[240]:


Sales.head()


# Describe data set

# ## 3 This is the R function we want to translate into Python

# In[ ]:





# In[241]:


# Applied to ONE univariate time series

MYDATA <- PLOT_LEAFLET_CDR_NUM %>% 
  select(Country,date,Confirmed) %>% 
  filter(Country=="France")


# In[ ]:


# Compute 80% train 20% test based on a 100 records
HUNDREC <- MYDATA[1:100,]

split_train_test <- function(INPUTDF,perctrain) {
  
  # Define set length for the TRAIN and TEST split  
  TROWS <- nrow(INPUTDF)
  TRAIN <- ceiling((TROWS*perctrain)/100)
  TEST  <- ceiling((TROWS*(100-perctrain)/100))
  ## Subset TRAIN data
  TRAIN_dataM <- INPUTDF[1:TRAIN,]
  TRAIN_data <-TRAIN_dataM %>% mutate(Splitd = "TRAIN")
  nrow(TRAIN_data)
  # Subset TEST data
  ST_TEST <- nrow(TRAIN_data)+1
  TEST_dataM  <- INPUTDF[ST_TEST:TROWS,]
  TEST_data <- TEST_dataM %>% mutate(Splitd = "TEST")
  # Final test dataset
  TEST_data
  # Append Train and Test split
  Split <-rbind.data.frame(TRAIN_data,TEST_data)
  # Return ouput from function
  return(Split)
}

# Important, always assign the function to an object to obtain a data frame
MYDATRAINT <-split_train_test(MYDATA,80)

# PLOT result


# Set of functions 

# - Number of rows () TROWS <- nrow(INPUTDF)

# In[242]:


Sales.head()


# In[243]:


index = Sales.index
number_of_rows = len(index)


# In[244]:


index


# In[245]:


number_of_rows


# ## 4 Building up the Python function

# ### 4.1 Train set 

# Python functions floor() and ceil() for flooring and ceiling numeric results

#   TRAIN <- ceiling((TROWS*perctrain)/100)

#   TROWS <- nrow(INPUTDF)

# In[246]:


TROWS = len(Sales.index)


# In[247]:


TROWS


# We define now the perctrain variable

# In[248]:


perctrain = 80


# In[249]:


perctrain


# In[250]:


import math


# In[251]:


TRAIN =   len(Sales.index)*perctrain    


# In[252]:


TRAIN =   (len(Sales.index)*perctrain  )/100


# In[253]:


TRAIN


# Then we finally have to apply the ceiling function in Python 

#   This is the R version of it: TRAIN <- ceiling((TROWS*perctrain)/100)

# In[254]:


TRAIN = (len(Sales.index)*perctrain)


# In[255]:


TRAIN = math.ceil(((len(Sales.index)*perctrain)/100))


# In[256]:


TRAIN


# This is an example on how cailing and floor functinos work

# In[257]:


TRAIN_ceil = math.ceil(((len(Sales.index)*perctrain)/100))


# In[258]:


TRAIN_ceil


# In[259]:


TRAIN_floor = math.floor(((len(Sales.index)*perctrain)/100))


# In[260]:


TRAIN_floor


# splitting a number into the integer and decimal parts

# In[261]:


val_int = int(TRAIN)
val_fract = TRAIN - val_int


# In[262]:


val_int


# In[263]:


val_fract


# In[ ]:





# ### 4.2 Test set 

#   This is the R version of it:   TEST  <- ceiling((TROWS*(100-perctrain)/100))

# In[264]:


perctrain = 80


# In[265]:


TRAIN_floor = math.floor(
    (
 ( 
        (len(Sales.index)*perctrain-1)
  )                        
                          /100)
)+1


# In[266]:


TRAIN_floor


# We need to add 100-percentain

# In[267]:


100-perctrain


# In[268]:


TRAIN_floor = math.floor(((len(Sales.index)*perctrain-1)/100))


# In[269]:


TRAIN_floor


# Adding required changes 100-perctrain

# In[270]:


TEST_floor = math.floor(((len(Sales.index)*
                           
                          (100- perctrain) 
                          
                          
                          )/100))+1


# In[271]:


TEST_floor


# Check that all ads upp

# In[272]:


len(Sales)


# In[273]:


TRAIN_floor


# In[274]:


TEST_floor


# In[275]:


Check_spllit = TRAIN_floor + TEST_floor


# In[276]:


Check_spllit


# ### 4.3 Subset TRAIN data

# This is the code we want to translate from R

# - Subset TRAIN data

#   TRAIN_dataM <- INPUTDF[1:TRAIN,]
#   TRAIN_data <-TRAIN_dataM %>% mutate(Splitd = "TRAIN")
#   nrow(TRAIN_data)

# I need to use iloc to slice Train data set

# In[277]:


TRAIN_floor


# Important, remember to enclose in brackets [ ] when slicing using .iloc[] any data frame

# In[278]:


TRAIN = Sales.iloc[0:TRAIN_floor,]


# In[279]:


TRAIN


# In[280]:


TRAIN_test = TRAIN.copy()


# Then I need to add a new variable that will flag this dataset as Train dataset

# We add a new column by using the square braket notation

# In[281]:


TRAIN_test['Train set'] = 'Train'


# In[282]:


TRAIN_test


# In[283]:


TRAIN_test.tail()


# In[284]:


len(TRAIN_test)


# ### 4.4 Subset TEST data

# In[285]:


TROWS = len(Sales.index)


# This is the code I use in R to translate into Python 

# - Subset TEST data
#  - ST_TEST <- nrow(TRAIN_data)+1
#  - TEST_dataM  <- INPUTDF[ST_TEST:TROWS,]
#  - TEST_data <- TEST_dataM %>% mutate(Splitd = "TEST")
#  -  Final test dataset
#  - TEST_data

# In[286]:


TROWS = len(Sales.index)


# In[287]:


TROWS


# We define the ST_TEST by counting number of rows for TRAIN plus one

# In[288]:


len(TRAIN)


# In[289]:


ST_TEST = len(TRAIN)


# In[290]:


ST_TEST


# Comparison between R and Python scripts

# - This is the R scriot
#  - TEST_dataM  <- INPUTDF[ST_TEST:TROWS,]

# - This is the Python scriot
#  - TEST_dataM = Sales.iloc[ST_TEST:,]

# This is how you translate above line into Python. Important we get the split for TEST from the **original** data set called **Sales**

# In[291]:


TEST_dataM = Sales.iloc[ST_TEST:,]


# In[292]:


TEST_dataM


# Now this setup is perfect

# Create new variable to identify Test rows

# In[293]:


TEST_dataM['Train set'] = 'Test'


# In[294]:


TEST_dataM


# In[295]:


TRAIN_test.head()


# In[296]:


TEST_dataM.head()


# Rename Test data set accordingly

# In[297]:


TEST = TEST_dataM


# In[298]:


TEST


# In[299]:


TRAIN = TRAIN_test


# In[300]:


TRAIN


# We can then append both Train and Test data sets

# #### 4.5 How to append data frames and lists 

# We use the append() function to stack on top of each other two data frames that have the same variable name, they can be of different length, but must share the same column names 

# #### Appendinng lists

# In[301]:


a = ["apple", "banana", "cherry"]
b = ["Ford", "BMW", "Volvo"]


# In[302]:


a.append(b)
print(a)


# #### Appendinng data frames

# In[303]:


https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.append.html


# Appending rows to a DataFrame can be more computationally intensive than a single concatenate. A better solution is to append those rows to a list and then concatenate the list with the original DataFrame all at once.

# In[304]:


df = pd.DataFrame([[1, 2], [3, 4]], columns=list('AB'), index=['x', 'y'])
df


# In[305]:


df2 = pd.DataFrame([[5, 6], [7, 8]], columns=list('AB'), index=['x', 'y'])
df2


# In[306]:


df3 = df.append(df2)
df3


# The append function also works with diiferent indexes

# In[307]:


df3 = pd.DataFrame([[5, 6], [7, 8]], columns=list('AB'), index=['m', 'n'])
df3


# In[308]:


df4 = pd.DataFrame([[8, 7], [4, 1]], columns=list('AB'), index=['g', 'w'])
df4


# In[309]:


DF34 = df3.append(df4)
DF34


# ### 4.6 Appen Train and Test data sets

# In[337]:


Dataset = TRAIN.append(TEST)


# In[338]:


Dataset.reset_index()


# 1. Important set PERIOD to datetime

# In[339]:


Dataset['PERIOD'] = pd.to_datetime(Dataset['PERIOD'])


# In[340]:


Dataset


# What type  of object is this?

# In[332]:


print(type(Dataset))


# 2. Set PERIOD as Index

# In[341]:


Dataset = Dataset.set_index('PERIOD')


# In[342]:


Dataset


# Important when selecting variables you **CANNOT** select the index variable (it will give you an error

# In[359]:


EXPdata = Dataset[['Sales','Train set']]


# In[360]:


EXPdata


# In[345]:


print(type(EXPdata))


# - Select rows from Data frame based on  column values 

# 1. Select Train data 

# In[ ]:


df.loc[df['column_name'] == some_value]


# In[364]:


EXPdataTRAIN = EXPdata.loc[EXPdata['Train set'] == 'Train']


# In[365]:


EXPdataTRAIN


# In[371]:


EXPdataTRAINplot =  EXPdataTRAIN['Sales']


# In[376]:


EXPdataTRAINplot


# 2. Select Test data 

# In[366]:


EXPdataTEST = EXPdata.loc[EXPdata['Train set'] == 'Test']


# In[370]:


EXPdataTEST


# In[369]:


EXPdataTESTplot =  EXPdataTEST['Sales']


# In[374]:


EXPdataTESTplot


# ### 5.Transform Series objects to dataframe objects

# In[385]:


traindataf = EXPdataTRAINplot.to_frame().apply(np.int64)


# In[386]:


testdataf = EXPdataTESTplot.to_frame().apply(np.int64)


# In[390]:


traindataf


# In[391]:


testdataf


# Check length

# In[394]:


len(traindataf)


# In[395]:


len(testdataf)


# ### 6. Plot it

# We want to use matplotlib to plot train and test sets

# In[372]:


import matplotlib.pyplot as plt


# In[396]:


plt.figure(figsize=(15,6))
plt.title('Total emergency admissions')
plt.plot(traindataf, label = "Test data")
plt.ylabel("value")
plt.xlabel("days")
plt.show()


# Important before plotting anything turn PERIOD into DATE format

# In[ ]:


plt.figure(figsize=(15,6))
plt.title('Total emergency admissions')
plt.plot(traindataf, label = "Train data")
plt.plot(valdataf, label = "Validation data")
plt.plot(holdf, label = "Hold data")
plt.legend()
plt.ylabel("value")
plt.xlabel("days")
plt.show()


# This is the commpleted workbook to create function to spit between train and test data
