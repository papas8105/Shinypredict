#!/usr/bin/env python
# coding: utf-8

# # Adhoc functions

# This workbook includes several adhoc functions to be used on different scripts

# In[2]:


import pandas as pd
import numpy as np
import os


# In[3]:


import matplotlib.pyplot as plt
import seaborn as sns


# In[4]:


path = os.getcwd()
path


# ### 1. Split initial data into Train and Test sets

# We have designed a function to slpit initial input data frame into Train and Test sets. 

# The function has two paramters: 
# - The data set you want to split defined by the "Inputdata" object
# - The percentage you want to assign to the Train split (usuallt 80%)

# In[6]:


os.chdir('C:/Pablo UK/46 DATA SCIENCE all/10 ML python JULY 2021')


# Read in dataset

# In[7]:


Sales = pd.read_csv('Sales_to_split_TRAIN_TEST.csv') 
Sales


# In[8]:


def traintestsplitn(Inputdata,perctrain):
    index = Inputdata.index
    number_of_rows = len(index)
   
    TROWS = len(Inputdata.index)
    
    TRAIN_ceil = math.ceil(((len(Inputdata.index)*perctrain)/100))
    TRAIN = Inputdata.iloc[0:TRAIN_ceil,]
    TRAIN_output = pd.DataFrame(TRAIN)
    TRAIN_output.to_csv('TRAIN.csv', index=False)
        
    ST_TEST = len(TRAIN)
    
    TEST = Inputdata.iloc[TRAIN_ceil:,]
    TEST_output = pd.DataFrame(TEST)
    TEST_output.to_csv('TEST.csv', index=False)
    return TRAIN_output,TEST_output


# In[10]:


import math


# In[11]:


traintestsplitn(Sales,80)


# ### Then we read in TRAIN and TEST files produced

# In[12]:


TRAIN = pd.read_csv('TRAIN.csv')
TRAIN


# In[13]:


TEST = pd.read_csv('TEST.csv')
TEST


# In[14]:


print(isinstance(TRAIN, pd.DataFrame))
print(isinstance(TEST, pd.DataFrame))


# ## 2. Check original file length matches TRAIN and TEST split

# In[ ]:


len(Sales)
len(TRAIN)
len(TEST)


# In[ ]:


len(TRAIN)+len(TEST)


# Function to check for the right lenght

# In[16]:


def chcksplit(main,traind,testd):
    if len(main)== len(traind)+len(testd):
        print('The split has been successful')
    elif len(main)!= len(traind)+len(testd):
         print('Try again')


# In[17]:


chcksplit(Sales,TRAIN,TEST)


# ## 3. Plot train and test series

# **Importnat, set DATE as Index for both TRAIN and TEST sets**

# In[19]:


TRAIN_plot = TRAIN.set_index('DATE')
TRAIN_plot


# In[23]:


TEST_plot = TEST.set_index('DATE')
TEST_plot.head()


# In[54]:


plt.figure(figsize=(40,20))
plt.title('Total sales')
plt.plot(TRAIN_plot, label = "Train data")
plt.plot(TEST_plot, label = "Test data")
plt.legend()
plt.ylabel("value")
plt.xlabel("years")
plt.tick_params(axis='x', which='major', labelsize=3)
plt.show()


# ## 4. Check distribution of each data sets

# #### 4.1  Train set

# In[57]:


plt.figure(figsize=(20,10))
plt.title('Total sales')
plt.plot(TRAIN_plot, label = "Train data")
plt.legend()
plt.ylabel("value")
plt.xlabel("years")
plt.tick_params(axis='x', which='major', labelsize=3)
plt.show()


# #### 4.2  Test set

# In[58]:


plt.figure(figsize=(20,10))
plt.title('Total sales')
plt.plot(TEST_plot, label = "Test data")
plt.legend()
plt.ylabel("value")
plt.xlabel("years")
plt.tick_params(axis='x', which='major', labelsize=3)
plt.show()


# In[ ]:




