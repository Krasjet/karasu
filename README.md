karasu
======

[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/Krasjet/karasu/build)](https://github.com/Krasjet/karasu/actions?query=workflow%3Abuild)

[karasu][karasu] is a self-hosted remote communication system designed for math
discussions. karasu is expected to be accompanied by [kamome][kamome], the
front-end components, i.e. the editor and style­sheets for karasu.

[karasu]: https://github.com/Krasjet/karasu
[kamome]: https://github.com/Krasjet/kamome

Due to the limitations of the medium. Please see [**here**][karasu_intro] for
detailed introduction.

[karasu_intro]: https://krasjet.com/voice/karasu/

Install
-------

Docker compose should be able to take care of everything.

```
$ git clone --recursive https://github.com/Krasjet/karasu.git
$ cd karasu
$ cp .env.example .env
$ docker-compose up
```

Note that the default fonts in [kamome][kamome] is a set of alternative free
fonts. If you want to use the ones I'm currently using, you need to
purchase/download the fonts listed [here][fonts] and set the `USE_ALT_FONTS`
option in `docker-compose.yml` to `false`.

[fonts]: https://github.com/Krasjet/kamome/tree/master/static/fonts

Screenshots
-----------

### Editor

![editor](https://krasjet.com/voice/karasu/imgs/editor.png)

### Fullscreen preview

![preview](https://krasjet.com/voice/karasu/imgs/view.png)
