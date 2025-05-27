library(jsonlite)
library(httr)

assume_role <- function(profile_name, role_arn) {
  # Get temporary credentials using AWS CLI
  cmd <- sprintf("aws sts assume-role --role-arn %s --role-session-name RSession --profile %s", role_arn, profile_name)
  creds_json <- system(cmd, intern = TRUE)
  
  # Parse JSON output
  creds <- fromJSON(paste(creds_json, collapse = ""))
  
  # Set environment variables
  Sys.setenv(
    AWS_ACCESS_KEY_ID = creds$Credentials$AccessKeyId,
    AWS_SECRET_ACCESS_KEY = creds$Credentials$SecretAccessKey,
    AWS_SESSION_TOKEN = creds$Credentials$SessionToken
  )
  
  message("Successfully assumed role: ", role_arn)
}

# Example usage
assume_role("ellipse-dev-enduser", "arn:aws:iam::097610011506:role/TARGET_ROLE")

