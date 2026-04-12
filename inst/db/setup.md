# Preparation

- conceptually what we are setting up here is the pathway: Particle Webhook → API Gateway → Lambda → RDS Proxy → RDS
- we'll set this up from back to front (RDS first, Webhook last)
- log into aws with your user access key and secret: `aws configure`

# Network

## Create VPC (virtual private cloud)

- check if we already have a default VPC (usually yes): `aws ec2 describe-vpcs` --> find the `VpcId` (starts with `vpc-...`)
- then check for the subnets (replace `{{VpcId}}`): `aws ec2 describe-subnets --filters "Name=vpc-id,Values={{VpcId}}" --query "Subnets[*].{SubnetId:SubnetId,AZ:AvailabilityZone,CIDR:CidrBlock}"`
- note down 3 `SubnetId`s for the next step (doesn't really matter which)

## Create DB subnet of the VPC

- check if we already have one (probably not): `aws rds describe-db-subnet-groups`
- with the 3 `SubnetId`s from the previous step (replace the `{{SubnetIdX}}` placeholders), run the following to create the DB subnet:
- `aws rds create-db-subnet-group --db-subnet-group-name kopflab-db-subnet-group --db-subnet-group-description "KopfLab DB subnet group" --subnet-ids {{SubnetId1}} {{SubnetId2}} {{SubnetId3}}`

## Create the security group for the database

- with the `VpcId` from earlier, run this: 
- `aws ec2 create-security-group --group-name kopflab-rds-sg --description "KopfLab RDS security group" --vpc-id {{VpcId}}`
- note the resulting `GroupId`
- allow everything inside the VPC to reach the postgres DB (port 5432):
- `aws ec2 authorize-security-group-ingress --group-id <<GroupId>> --protocol tcp --port 5432 --cidr 172.31.0.0/16`
- allow admin IP (figure out your IP and replace `<<IP>>` placeholder) to access the DB:
- `aws ec2 authorize-security-group-ingress --group-id <<GroupId>> --protocol tcp --port 5432 --cidr <<IP>/32`

# Database

## Create instance

- create the `kopflab-db` database instance with `microloggers` as DB (can add others), avoid `@`, `'`, `/`, and `@` in the `{{Pwd}}`: 

```bash
aws rds create-db-instance \
  --db-instance-identifier kopflab-db \
  --db-instance-class db.t3.small \
  --engine postgres \
  --master-username postgres \
  --master-user-password '{{Pwd}}' \
  --allocated-storage 20 \
  --db-subnet-group-name kopflab-db-subnet-group \
  --vpc-security-group-ids {{VpcId}} \
  --db-name microloggers \
  --publicly-accessible
```

- it may take some time to create, check if it's available with `aws rds describe-db-instances --db-instance-identifier kopflab-db --query "DBInstances[0].{Status:DBInstanceStatus,Endpoint:Endpoint.Address,Port:Endpoint.Port}"`
- once it is available, note down the `Endpoint`

## Create database and user

- install postgresql to be able to run `psql`: `brew install postgresql`
- connect to the database (`microloggers` here) with the `postgres` admin user (replace `{{Endpoint}}` with the `Endpoint` from the previous step): `psql -h {{Endpoint}} -U postgres -d microloggers`
- create a user with the same name with privileges for just that database

```bash
CREATE USER microloggers WITH PASSWORD '{{Pwd}}';
GRANT CONNECT ON DATABASE microloggers TO microloggers;
GRANT CREATE ON SCHEMA public TO microloggers;
GRANT USAGE ON SCHEMA public TO microloggers;
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO microloggers;
ALTER DEFAULT PRIVILEGES IN SCHEMA public
  GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO microloggers;
ALTER DATABASE microloggers OWNER TO microloggers;
```

- log into the database SQL console with the new `microloggers` user: `psql -h {{Endpoint}} -U microloggers -d microloggers`
- run the SQL scripts in [create.sql] to create all the tables
- for testing the lambda function, also run the [testing.sql]

## Store credentials in Secrets Manager

- for lambda functions and others to access the DB, store the specific user credentials in a secret (replace `{{Pwd}}` and `{{Endpoint}}`):

```bash
aws secretsmanager create-secret \
  --name kopflab/db-credentials/microloggers \
  --description "KopfLab RDS credentials" \
  --secret-string '{
    "username": "microloggers",
    "password": "{{Pwd}}",
    "host": "{{Endpoint}}"
  }'
```

## Create the proxy for connection management

This is not strictly necessary but particularly useful for the lambda functions so they don't need to create individual connections each time but reuse a connection pool. The previously created DB instance endpoint is still the one to use to make local connections (the proxy only works indide the VPC).

- create an IAM role for the proxy:

```bash
aws iam create-role \
  --role-name kopflab-rds-proxy-role \
  --assume-role-policy-document '{
    "Version": "2012-10-17",
    "Statement": [{
      "Effect": "Allow",
      "Principal": {"Service": "rds.amazonaws.com"},
      "Action": "sts:AssumeRole"
    }]
  }'
```

- attach a policy to the role to allow access to the secrets

```bash
aws iam put-role-policy \
  --role-name kopflab-rds-proxy-role \
  --policy-name kopflab-rds-proxy-secret-access \
  --policy-document '{
    "Version": "2012-10-17",
    "Statement": [{
      "Effect": "Allow",
      "Action": "secretsmanager:GetSecretValue",
      "Resource": "arn:aws:secretsmanager:us-east-1:713881824347:secret:kopflab/db-credentials/microloggers*"
    }]
  }'
```

- confirm the policy was attached with `aws iam get-role-policy --role-name kopflab-rds-proxy-role --policy-name kopflab-rds-proxy-secret-access`
- create the proxy adding in the security `GroupId` and `SubnetId1`, `SubnetId2`, `SubnetId3`:

```bash
aws rds create-db-proxy \
  --db-proxy-name kopflab-rds-proxy \
  --engine-family POSTGRESQL \
  --auth '[{
    "AuthScheme": "SECRETS",
    "SecretArn": "arn:aws:secretsmanager:us-east-1:713881824347:secret:kopflab/db-credentials/microloggers",
    "IAMAuth": "DISABLED"
  }]' \
  --role-arn arn:aws:iam::713881824347:role/kopflab-rds-proxy-role \
  --vpc-subnet-ids {{SubnetId1}} {{SubnetId2}} {{SubnetId3}} \
  --vpc-security-group-ids {{GroupId}}
```

- note the proxy endpoint for later use as `ProxyEndpoint`
- register the RDS instance as the proxy's target: `aws rds register-db-proxy-targets --db-proxy-name kopflab-rds-proxy --db-instance-identifiers kopflab-db`
- check if the proxy is available with `aws rds describe-db-proxies --db-proxy-name kopflab-rds-proxy --query "DBProxies[0].Status"`
- check if the proxy target is available with `aws rds describe-db-proxy-targets --db-proxy-name kopflab-rds-proxy --query "Targets[0].TargetHealth"`

## Update credentials with proxy endpoint

- make sure the proxy endpoint is also available in the secret for the lambda function to connect to it (replace `{{Pwd}}`, `{{Endpoint}}`, `{{ProxyEndpont}}`):

```bash
aws secretsmanager create-secret \
  --name kopflab/db-credentials/microloggers \
  --description "KopfLab RDS credentials" \
  --secret-string '{
    "username": "microloggers",
    "password": "{{Pwd}}",
    "host": "{{Endpoint}}",
    "proxy": "{{ProxyEndpoint}}"
  }'
```

# Next steps

Set up the lambda function and gateway, see `lambda` folder.