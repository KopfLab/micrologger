# Particle Webhook

For the particle events (here `microloggerData` or whatever your sdds device has set as its data event) to get forwarded to the API Gateway, copy/rename the `webhook_template.json` to `webhook.json` (git-ignored) and configure the `webhook.json` with the `API-ID` and `API-KEY` from the lambda setup and run:

-`particle webhook create webhook.json` (need to be logged into the particle account via `particle login`)
- note that creating the webhooks multiple times will NOT throw an error but lead to multiple trigger events, make sure to remove old webhooks via `particle webhook list` and `particle webhook delete` before re-creating updated versions

