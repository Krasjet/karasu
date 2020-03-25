# karasu

[karasu](https://github.com/Krasjet/karasu) is a self-hosted remote communication system designed for mathematicians and myself. karasu is expected to be accompanied by [kamome](https://github.com/Krasjet/kamome), the editor and style­sheets for karasu.

Due to the limitations of GitHub's Markdown. Please see [**here**](https://krasjet.com/voice/karasu/) for detailed introduction.

## Install

Due to the use of licensed fonts, you need to find proper replacement for the [fonts used in kamome](https://github.com/Krasjet/kamome/tree/master/static/fonts) first. Except this, docker compose should be able to take care of everything.
```
$ git clone https://github.com/Krasjet/karasu.git
$ cd karasu
$ cp .env.example .env
$ docker-compose up
```

## Screenshots

### Editor

![editor](https://krasjet.com/voice/karasu/imgs/editor.png)

### Fullscreen preview

![preview](https://krasjet.com/voice/karasu/imgs/view.png)
