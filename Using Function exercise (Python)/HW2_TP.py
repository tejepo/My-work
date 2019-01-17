{
 "cells": [
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "import pandas as pd\n",
    "import numpy as np\n",
    "\n",
    "file = 'https://data.lacity.org/api/views/nxs9-385f/rows.csv?accessType=DOWNLOAD'\n",
    "\n",
    "#This data set comes from the 2010 Census Profile of General Population and Housing Characteristics in LA.\n",
    "data = pd.read_csv(file, index_col = 0) #setting the index for this data set to the first column (which are the zip codes)\n",
    "data.head()"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "expected_cols = [\"Total Population\", \"Median Age\", \"Total Males\", \"Total Females\", \"Total Households\", \"Average Household Size\"]\n",
    "\n",
    "def test_create_dataframe(dataframe):\n",
    "    #This block will compare the strings in the object 'expected_cols' to a list of the dataframe's column names.\n",
    "    if expected_cols == list(dataframe):\n",
    "        print(\"Column headers, confirmed\")\n",
    "    else:\n",
    "        print(\"Error: Please check column names.\")\n",
    "        return print(\"Please address before continuing.\")\n",
    "    \n",
    "    #This block will iterate across columns in the dataframe and check whether the data type is equal to float or int\n",
    "    for y in dataframe.columns:\n",
    "        if(dataframe[y].dtype == np.float64 or dataframe[y].dtype == np.int64):\n",
    "            print(\"Numeric data type for column: '%s,' confirmed.\" %(y))\n",
    "        else:\n",
    "            print(\"Error: Non-numeric data types detected. Please examine data type of column: '%s'\" %(y))\n",
    "            return print(\"Please address before continuing.\")\n",
    "        \n",
    "    #This block compares the length of the indext to a value of 10\n",
    "    if len(dataframe.index) >= 10:\n",
    "        print(\"Dataset minimum rows, confirmed. There are %d rows in this data set.\" %(len(dataframe.index)))\n",
    "    else:\n",
    "        print(\"Error: Not enough rows. To continue, please increase the number of rows by %d.\" %(10-len(dataframe.index)))\n",
    "        return print(\"Please address before continuing.\")\n",
    "    \n",
    "    #Since the other blocks return error messages and end the sequence, this final block will only display when the above conditions are all cleared\n",
    "    if 1:\n",
    "        print(\"Boot sequence: Complete.\")\n",
    "        print(\"Homework assignment: Finished.\")"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "test_create_dataframe(data)"
   ]
  }
 ],
 "metadata": {
  "kernelspec": {
   "display_name": "Python 3",
   "language": "python",
   "name": "python3"
  },
  "language_info": {
   "codemirror_mode": {
    "name": "ipython",
    "version": 3
   },
   "file_extension": ".py",
   "mimetype": "text/x-python",
   "name": "python",
   "nbconvert_exporter": "python",
   "pygments_lexer": "ipython3",
   "version": "3.5.6"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 2
}
