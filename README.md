# Quandl-API-Coding-Test
Retrieves pricing data from the Quandl WIKI Stock Price API for a given set of securities and date range.  Includes functionality to return calculations against the data.

DOWNLOAD INSTRUCTIONS:

Choose the green Clone or Download button.
Select Download Zip.  Extract all into default folder.

REQUIREMENTS:

Program written for a Win32 environment.

A Delphi compiler is required to compile the code.  
A free demo can be downloaded from :https://www.embarcadero.com/products/delphi
Follow the compiler installation instructions.

Open CapitalOneInvestingCodingProject.groupproj from your Delphi compiler.
Build and run each of the projects included in the group project.

Internet connection is required to extract data from Quandl.  Otherwise, the program will prompt you for a file.  Choose the file WIKI-PRICES.csv, located in your expanded zip folder.

USER GUIDE:

CapitalOneInvestingCodingTest:

To run the program, double click CapitalOneInvestingCodingTest.exe.
To test functionality, check all of the checkboxes.  The program opens with suggested security ticker codes and a suggested date range.  These can be changed.  Ticker codes must be separated by commas.  When ready, press the Run Quandl button.  The results for all of the methods checked will be displayed in the text box below.

TestSecurityUtilityProject:

To run the DUnit test, double click TestSecurityUtilityProject.exe.
This program runs tests for the methods BusyDay and MonthlyAverageValues from the SecurityUtilityUnit.pas.
