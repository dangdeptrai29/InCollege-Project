# InCollege Project – Week 3 Deliverable

## Description
This repository contains the Week 3 deliverable for the InCollege Project.  
The objective of this milestone is to implement the **core authentication system** and simulate the initial user navigation menu in COBOL.  

The program provides the following functionality:
- **User Registration**: New users can create an account (up to 5 accounts).  
  - Passwords must be 8–12 characters long and include at least one capital letter, one digit, and one special character.  
  - Accounts are persisted in a file and automatically reloaded when the program restarts.  
  - On the 6th account attempt, the system displays:  
    `"All permitted accounts have been created, please come back later."`  

- **User Login**: Returning users can log in using existing credentials.  
  - Successful login → `"You have successfully logged in."`  
  - Failed login → `"Incorrect username/password, please try again."` (unlimited attempts allowed).  

- **Post-Login Navigation**:  
  - Options presented: **Search for a job**, **Find someone you know**, **Learn a new skill**.  
  - Job Search & Find Someone → `"Under construction"` message.  
  - Learn a new skill → Presents 5 skills to choose from (all return `"Under construction"`).  
  - Includes option to return to main menu.  

- **Input/Output Handling**:  
  - All input (username, password, menu selections) is read from an input file.  
  - All output is displayed on the screen **and** written to an output file for verification.  
  - Output file must exactly match console output.  

This milestone also includes tester responsibilities:
- Development of comprehensive test input files (positive, negative, edge cases).  
- Verification that outputs in console and output file match exactly.  
- Logging bugs in Jira when discrepancies are found.  
