Hi Talha, Just a few comments here.

I put these as a md file within the repository so that we can track them in the future.

When you review these, you can just erase the document. Github will still keep the document in the version control system.

# files
- See that you have moved the historical....file to the /Data folder. But there is still a copy of the file in the root folder. Please avoid to have two copies of the same document. It just leads to messiness and conflicts between version.

# Code
- If you are no longer using the code in lines 7-11 just erase it. Github will keep it in the version control.

- Reading files in line 26. It is OK for the moment. But later on, you will need to do more complex things.
	- At some point, we will have more than one building in the analysis
	- When we read data for longer periods, we may have several files for each building. Each with 1_3 months of data.
	- we will deal with it when the moment comes.

- Avoid hardcoding. i.e. the MWh to kWh conversion. See how I have parametrized this. Line 3 and 54.

- Readability. Avoid long lines. See my changes to line 49.

- Repetition and compacity. Repeating the same operation line by line is a source of errors. Try to put repeated lines into a for loop. i.e the content of lines 38-41, 70-77, 80-82. also, when you are plotting the same information for two heat pumps.

# Plots
- It is very nice how you have created the hourly plots

- Try to create a similar plot by day of the week

- Try to automaticall export the plots to files.

# Engineering analysis

- Electricity and Heat signals do not seem to be in the same units. This is very relevant to review. Maybe it is good that before going into details with the disaggregated data, hourly data,... you just take the total volume of electricity and heat for a period and calulate de COP. (HEATING+COOLING)/ELECTRICITY should be between 1 and 10.