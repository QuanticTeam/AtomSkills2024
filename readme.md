
## Local run

> docker compose up -d

> http://localhost:5005/

> https://localhost:443
>   * Allow self-signed certificate

Check backend:
> curl -X 'POST' 'http://localhost:5003/Something/Test'


### PGAdmin
http://localhost:15432/login

`admin@pgadmin.com`

### MINIO
http://localhost:9001/login

`minioadmin`

## Rebuild docker containers

> docker compose up -d --build