---
title: Installation
layout: base
---

# Installation

To compile and run STUnD directly on your computer, you can use either [the Haskell Tool Stack](https://docs.haskellstack.org/en/stable/) or build and run it inside a [Docker](https://www.docker.com/) container.

## Installation via Stack
If you have Stack, run  

```
stack build
```

or

```
stack install
```

which also installs the executable.

Afterwards you can run STUnD either with `stack run` from within the same folder or, if you have installed the executable, by running that directly.

## Using Docker
If you want to use Docker containers, the simplest way is to use `docker compose`. To build and run the image you can simply type:

```
docker compose up stund-gui
```

This will take a while for the first time because the image has to be built. Afterwards, running the container can be started directly with the same command.