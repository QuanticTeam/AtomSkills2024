
## Local run

> docker compose up -d

> https://localhost:5004/
>   * Allow self-signed certificate

Check backend:
> curl -X 'POST' 'https://localhost:5003/Something/Test'


### PGAdmin
http://localhost:15432/login

`admin@pgadmin.com` / `password`

### Logins:

  mentor | mentor
  student1 | student1
  student2 | student2

### MINIO
http://localhost:9001/login

`minioadmin`

## Rebuild docker containers

> docker compose up -d --build

## Test through the [hurl](hurl.dev)

> hurl --test ./src/test/healthy.hurl

## Wipe everything

docker system prune -a
