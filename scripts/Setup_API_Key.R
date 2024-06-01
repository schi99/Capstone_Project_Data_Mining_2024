# Setup_Key ---------------------------------------------------------------


# Create_a_Renviron_file --------------------------------------------------

# Use the Windows Editor App to create a file where on the first line 
# (without any spaces you write):
# Key=Your_API_KEY and save it to your working directory.

#  Call_the_Key_from_the_.Renviron-File -----------------------------------

# Use the follwing code to accese the Key 

api_key <- Sys.getenv(x= "Key")

# Before you push check the gitignore-file (if the .Renviron-file is there you
# can push and commit)

