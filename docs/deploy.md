
## Инструкция по установке

Установка поддерживает операционные системы

    * Linux
    * Windows

### Шаги установки


1. Install docker

    A. Для Linux

        1. Установить докер (варианты а или б на выбор):

            а). Запустить скрипт `sh install/docs/install.sh` предоставив пароль пользователя

            б). По инструкции: https://docs.docker.com/engine/install/ubuntu/#install-using-the-repository

    B. Windows

        1. Install WSL
            1. PowerShell: `wsl --install -d Ubuntu`
            2. Reboot system: `shutdown -r -t 0`
            3. Wait for `Installing, this may take a few minutes ...`
            4. Ввести любое имя и пароль пользователя внутри WSL машины
            5. Дождаться окончания выполнения команды
            6. Закрыть консоль Ubuntu

        2. Установить Docker Desktop 
            1. PowerShell: `winget install Docker.DockerDesktop`
            2. Reboot system: `shutdown -r -t 0`
            3. Запустить `Docker Desktop` и завершить настройку:
                * принять лицензию
                * залогиниться в докер или пропустить
                * пропустить опрос

2. Запустить консоль в папке с дистрибьютивом `team10/install`

3. Если требуется - обновить файлы сертификата в папке `https`
    
4. Если требуется - обновить набор обучающих материалов в папке `lessons`

5. Если требуется - обновить файл словаря дефектов `lessons\feature.json`

6. Для настройки автоматической проверки нужно:

    1. Открыть файл `docker-compose.yml'

    2. Настроить переменные окружения для модуля автоматической разметки:
        * AiOptions__AS_2024_ENV_HOST: "http://**XXX.XXX.XXX.XXX**"****
            > заменить XXX.XXX.XXX.XXX на IP адрес или имя хоста сервера, который используется для соединения с AI модулем проверки
        * AiOptions__AS_2024_ENV_PORT: **"10240"**
            > если требуется - заменить на порт, который используется для соединения с AI модулем проверки

7. Запустить команду
    
    1. Linux
        
        `sudo docker compose up -d`
    
    2. Windows
        
        PowerShell: `docker compose up -d`

8. Дождаться статуса всех контейнеров `Started` или `Healthy` (3-10 минут)

9. Дождаться чтобы контейнер с именем `deploy` был в статусе `Exited` (2-5 минут)

    1. Linux

        `sudo docker inspect deploy | grep Status`

    2. Windows

        PowerShell: `docker inspect deploy | select-string status`


### Проверка что решение развёрнуто корректно

1. Перейти в браузере по адресу https://localhost:5004

2. Разрешить самоподписанный сертификат (добавить исключение)

    * `Дополнительно` => `Принять риск и продолжить`

    
