# Lambda function

This assumes the lambda function is already developed and works with local invocation.

## Create execution role

- create an IAM role for the lambda functions:

```bash
aws iam create-role \
  --role-name kopflab-lambda-role \
  --assume-role-policy-document '{
    "Version": "2012-10-17",
    "Statement": [{
      "Effect": "Allow",
      "Principal": {"Service": "lambda.amazonaws.com"},
      "Action": "sts:AssumeRole"
    }]
  }'
```

- attach necessary policies:
- 1. Basic Lambda execution: `aws iam attach-role-policy --role-name kopflab-lambda-role --policy-arn arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole`
- 2. Access to the VPC: `aws iam attach-role-policy --role-name kopflab-lambda-role --policy-arn arn:aws:iam::aws:policy/service-role/AWSLambdaVPCAccessExecutionRole`
- 3. Secrets access for DB credentials: 

```bash
aws iam put-role-policy \
  --role-name kopflab-lambda-role \
  --policy-name kopflab-lambda-secret-access \
  --policy-document '{
    "Version": "2012-10-17",
    "Statement": [{
      "Effect": "Allow",
      "Action": "secretsmanager:GetSecretValue",
      "Resource": "arn:aws:secretsmanager:us-east-1:713881824347:secret:kopflab/db-credentials/microloggers*"
    }]
  }'
```

## Deploy

- deploy the lambda function with `sam build && sam deploy` (configuration comes from samconfig.toml)
- check that deployment worked and fetch API endpoint with `aws cloudformation describe-stacks --stack-name kopflab-micrologger --query "Stacks[0].Outputs"`
- the endpoint will have the format `{{API-ID}}.execute-api.us-east-1.amazonaws.com/Prod/micrologger` (see template.yaml `Outputs` section)
- note that the configuration in `template.yaml` also sets up the API gateway, though credentials still need ot be set up

# API gateway

## Create API key

- create an API key with: `aws apigateway create-api-key --name kopflab-particle-key --enabled`
- make sure to save the `id` (`KEY-ID`) and `value` (`API-KEY`) from the API key

## Create usage plan

- create a usage plan with the `API-ID` from earlier: `aws apigateway create-usage-plan --name kopflab-usage-plan --api-stages apiId={{API-ID}},stage=Prod`
- note the `id` from the output (`USAGE-ID`)
- attach API key to the usage plan (note that `API_KEY` is a constant here not the earlier value from the key!):
- `aws apigateway create-usage-plan-key --usage-plan-id {{USAGE-ID}} --key-id {{KEY-ID}} --key-type API_KEY`
- the return's `value` should be the `API-KEY` value from the earlier API key generation, this is what we need to setup the webhook

## Test API Gateway

- run the following for each test file (`_tree.json`, `_values.json`, `_state.json`, `_burst.json`) to check the gateway:

```bash
curl -X POST \
  https://85tvw8fhm8.execute-api.us-east-1.amazonaws.com/Prod/micrologger \
  -H "x-api-key: eyM5ytETRB1eDXXs8Avvb3sVNkO5EnPjaTXAmHRy" \
  -H "Content-Type: application/json" \
  -d @sddsData_tree.json
```

- check the logs with `aws logs tail /aws/lambda/kopflab-micrologger --follow`

# Next step

Setup the particle webhook by following the webhook/README.md