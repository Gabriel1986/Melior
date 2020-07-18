module Server.Media.Library

open Microsoft.Extensions.Configuration
open Amazon.S3

//TODO: fix this mess
let createAmazonS3ServiceClient (_config: IConfiguration) =
    let config = new Amazon.S3.AmazonS3Config()
    config.ForcePathStyle <- true
    config.ServiceURL <- "http://localhost:4572"
    config.UseHttp <- true
    new AmazonS3Client (config) :> IAmazonS3
    //let awsOptions = config.GetAWSOptions()
    //awsOptions.CreateServiceClient<AmazonS3Client>()