# Readme

This folder contains the instructions to set up the lambda function and AWS gateway. 
It assumes that the DB setup and creation is already complete (see db/setup.md).

# Lambda function development

Note: this assumes that the DB setup and creation is already complete (see db/setup.md).

- you should already have aws (check with `aws --version`)
- install aws sam e.g. via `brew install aws-sam-cli` (check with `sam --version`)
- build the lambda function with `sam build` (uses a docker container, make sure docker is running)
- run the following to test the function for different payloads (note that these require the test records created by db/testing.sql):
- `sam local invoke --env-vars env.json --docker-network host --event sddsData_tree.json`
- `sam local invoke --env-vars env.json --docker-network host --event sddsData_values.json`
- `sam local invoke --env-vars env.json --docker-network host --event sddsData_state.json`
- `sam local invoke --env-vars env.json --docker-network host --event sddsData_burst.json`

# Setup

Follow-the steps in [setup.md] to set up the Lambda function and gateway.

# (Re)deployment

(Re)deploy the Lambda function with: `sam build && sam deploy`
