# Cobol_Statement
Edit a statement from an existing file (containing an account number and a name) and a system input file (movements).

Entering files : 
FCPTE : length 50 bytes, 10 numerics, 14 alphanumerics and a filler.
SYSIN file : contains the type of request (A or B) and the informations related.

Type A : Edit the statement using FCPTE, the name of the applicant and two account' numbers : one for the beginning of the search and 
        the other for the end.

Type B : Edit the statement using FCPTE, the name of the applicant and two account' names : one for the beginning of the search and 
        the other for the end.
        
This program also creates an ETATANO that summarizes the mistakes possible concerning the requests.
