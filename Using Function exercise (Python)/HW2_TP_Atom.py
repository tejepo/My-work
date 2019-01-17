import pandas as pd
import numpy as np

#Importing the file
file = 'https://data.lacity.org/api/views/nxs9-385f/rows.csv?accessType=DOWNLOAD'

#This data set comes from the 2010 Census Profile of General Population and Housing Characteristics in LA.
data = pd.read_csv(file, index_col = 0) #setting the index for this data set to the first column (which are the zip codes)
data.head()

#These are the names that SHOULD be my column names
expected_cols = ["Total Population", "Median Age", "Total Males", "Total Females", "Total Households", "Average Household Size"]

#Here's where we create the function that will take a named dataframe and check several parameters
def test_create_dataframe(dataframe):
    #This block will compare the strings in the object 'expected_cols' to a list of the dataframe's column names.
    if expected_cols == list(dataframe):
        print("Column headers, confirmed")
    else:
        print("Error: Please check column names.")
        return print("Please address before continuing.")

    #This block will iterate across columns in the dataframe and check whether the data type is equal to float or int
    for y in dataframe.columns:
        if(dataframe[y].dtype == np.float64 or dataframe[y].dtype == np.int64):
            print("Numeric data type for column: '%s,' confirmed." %(y))
        else:
            print("Error: Non-numeric data types detected. Please examine data type of column: '%s'" %(y))
            return print("Please address before continuing.")

    #This block compares the length of the indext to a value of 10
    if len(dataframe.index) >= 10:
        print("Dataset minimum rows, confirmed. There are %d rows in this data set." %(len(dataframe.index)))
    else:
        print("Error: Not enough rows. To continue, please increase the number of rows by %d." %(10-len(dataframe.index)))
        return print("Please address before continuing.")

    #Since the other blocks return error messages and end the sequence, this final block will only display when the above conditions are all cleared
    if 1:
        print("Boot sequence: Complete.")
        print("Homework assignment: Finished.")

#now to actually run the function
test_create_dataframe(data)
