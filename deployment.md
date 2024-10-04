# Deployment

There are several possible ways to deploy STUnD, depending on your use case and available resources. The main supported ones are local standalone setup and containerized setup. First you have to download or clone [the source code from Github](https://github.com/harisont/STUnD), afterwards you can follow the instructions below. Both methods have been tested on Linux but should me able to be adjusted to other operating systems. 

## Standalone

Especially for development and testing it makes most sense to run STUnD standalone locally. Before STUnD can be run it has to be compiled. The easiest way is to use [the Haskell Tool Stack](https://docs.haskellstack.org/en/stable/). Building using Cabal is not encouraged.

If you have set up Stack correctly you can simply build STUnD by running

```
stack build
```

After the build has succeeded you can simply run it using

```
stack run
```

## Containers

A second way of deploying STUnD is using containers. Two common container solutions are `Docker` and `Podman` (which is mostly compatible with `Docker`). The latter has the advantage that the containers can be more eaily run by ordinary users. The configuration is split into two files `Dockerfile` and `compose.yml`. The first file defines how the image containing `STUnD` can be built. The second one describes how the container can be built on top of the image using the `docker-compose` or `podman-compose` tool.

The easiest way is just to rely on this `compose` tool. You can use 

```
docker compose build # (or podman compose build)
``` 

to build the container and 

```
docker compose up -d # (or podman compose up -d)
```

to run the container in the background. Afterwards STUnD can be accessed directly using a webbrowser using the URL http://localhost:3000 (port 3000 is the default and can be changed in the `compose.yml` file).

You can check the status of the container using 

```
docker compose ps
```

and

```
docker compose down
```

can be used to stop the STUnD server.

For a more stable deployment we recommend using a reverse proxy between the user and STUnD (e.g., [Apache mod_proxy](https://httpd.apache.org/docs/2.4/mod/mod_proxy.html#proxypass) or [Nginx Reverse Proxy](https://docs.nginx.com/nginx/admin-guide/web-server/reverse-proxy/)).


### Some technical details

Docker runs containers as the `root` user and users have to be a member of the `docker` group (or similar, depending on the operating system) to be able to create and run containers. In contrast `Podman` runs containers as the user who created the container, which can lead to some restrictions on how containers can be configured. ⚠️Most importantly, Linux shuts down all processes of a user when logging out, this includes podman containers. To avoid this, linger has to be enabled using  `loginctl enable-linger`.⚠️
