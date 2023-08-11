# This function takes the `bucket` name argument and uses the paws package
# to create a new bucket with that name, a new user than can only access that
# but, and creates and retrieves a set of credentials (api key and secret)
# to use for that user
create_project_bucket <- function(bucket_name) {
  s3_svc <- paws::s3()
  s3_svc$create_bucket(Bucket = bucket_name)
  iam_svc <- paws::iam()
  user_name <- paste0(bucket_name, "_user")
  iam_svc$create_user(UserName = user_name)

  #Create a policy allowing the user to only access that bucket
  policy <- paste0('{"Version": "2012-10-17","Statement": [{"Effect": "Allow","Action": "s3:*","Resource": ["arn:aws:s3:::', bucket_name, '/*"]}]}')
  policy_name <- paste0(bucket_name, "_policy")
  iam_svc$create_policy(PolicyName = policy_name, PolicyDocument = policy)

  #Attach the policy to the user
  iam_svc$attach_user_policy(UserName = user_name, PolicyArn = paste0("arn:aws:iam::", Sys.getenv("AWS_ACCOUNT_ID"), ":policy/", policy_name))

  #Create an access key for the user
  access_key <- iam_svc$create_access_key(UserName = user_name)

  #Return the access key and secret
  return(list(access_key = access_key$AccessKey$AccessKeyId, secret = access_key$AccessKey$SecretAccessKey))


}

# This function takes the `bucket` name argument and uses the paws package to
# delete the bucket and user associated with that bucket
delete_project_bucket <- function(bucket_name) {
  s3_svc <- paws::s3()
  s3_svc$delete_bucket(Bucket = bucket_name)
  iam_svc <- paws::iam()
  user_name <- paste0(bucket_name, "_user")
  iam_svc$delete_user(UserName = user_name)

  #Delete the associated policy
  policy_name <- paste0(bucket_name, "_policy")
  iam_svc$delete_policy(PolicyArn = paste0("arn:aws:iam::", Sys.getenv("AWS_ACCOUNT_ID"), ":policy/", policy_name))


}
