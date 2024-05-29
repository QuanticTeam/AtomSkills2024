
## Local run

> docker compose up -d

> https://localhost:443
>   * Allow self-signed certificate

Check:
> curl -X 'POST' 'http://localhost:443/Something/Test'


### PGAdmin
http://localhost:15432 

`admin@pgadmin.com`

## Rebuild docker containers

> docker compose up --build