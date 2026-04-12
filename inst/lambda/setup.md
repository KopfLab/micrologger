# Lambda function

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

FIXME: continue here