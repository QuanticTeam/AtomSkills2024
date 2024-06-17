
## Инструкция по установке

1. Install docker
    1. Windows
        1. Install WSL
          1. PowerShell: `wsl --install -d Ubuntu`
          2. Reboot system: `shutdown -r -t 0`
          3. Wait for `Installing, this may take a few minutes ...`
          4. Ввести любое имя и пароль пользователя внутри WSL машины
          5. Дождаться окончания выполнения команды
          6. Закрыть консоль Ubuntu
      2. Установить Docker Desktop 
        1. > PowerShell: `winget install Docker.DockerDesktop`
        2. Reboot system: `shutdown -r -t 0`
        3. Запустить `Docker Desktop` и завершить настройку докера (принять лицензию, пропустить логин, пропустить опрос)
    2. Linux
        1. Установить докер (варианты а и б на выбор)
          a. По инструкции: https://docs.docker.com/engine/install/ubuntu/#install-using-the-repository
          б. Запустить скрипт `docs/install.sh` предоставив пароль пользователя

2. Запустить консоль в папке с исходным кодом
3. Если требуется заменить сертификаты в папке `https`
4. Запустить команду
  1. Windows
    `docker compose up -d`
  2. Linux
    `sudo docker compose up -d`
5. Дождаться статуса всех контейнеров `Started` или `Healthy` (3-10 минут в зависимости от окружения)
6. Перейти в браузере по адресу https://localhost:5004
    1. Разрешить самоподписанный сертификат (добавить исключение)
    * > "Дополнительно" -> "Принять риск и продолжить"
    
