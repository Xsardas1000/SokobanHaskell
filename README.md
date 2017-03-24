# sokoban

[![Build Status](https://travis-ci.org/cmc-haskell-2016/sokoban.svg?branch=master)](https://travis-ci.org/cmc-haskell-2016/sokoban)

## Установка и запуск

Для установки клонируйте репозиторий и соберите проект с помощью `stack`:

```
git clone https://github.com/cmc-haskell-2016/sokoban.git
cd sokoban
stack setup
stack build
```

После установки запуск осуществляется командой `stack exec`:

```
stack exec sokoban
```

Во время разработки инициировать повторную сборку проекта с последующим запуском рекомендуется
следующей командой:

```
stack build && stack exec sokoban
```
# SokobanHaskell
