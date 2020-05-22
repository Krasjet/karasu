# karasu

**Refactoring in progress**

[karasu](https://github.com/Krasjet/karasu) is a self-hosted remote communication system designed for mathematicians and myself. karasu is expected to be accompanied by [kamome](https://github.com/Krasjet/kamome), the editor and style­sheets for karasu.

Due to the limitations of GitHub's Markdown. Please see [**here**](https://krasjet.com/voice/karasu/) for detailed introduction.

## Install

Docker compose should be able to take care of everything.
```
$ git clone --recursive https://github.com/Krasjet/karasu.git
$ cd karasu
$ cp .env.example .env
$ docker-compose up
```

Note that the default fonts in [kamome](https://github.com/Krasjet/kamome) is a set of alternative free fonts. If you want to use the ones I'm currently using, you need to purchase/download the fonts listed [here](https://github.com/Krasjet/kamome/tree/master/static/fonts) and set the `USE_ALT_FONTS` option in `docker-compose.yml` to `false`.

## Screenshots

### Editor

![editor](https://krasjet.com/voice/karasu/imgs/editor.png)

### Fullscreen preview

![preview](https://krasjet.com/voice/karasu/imgs/view.png)
