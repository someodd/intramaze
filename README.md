# Interwebz

Random chat encounters in a collaboratively-built maze.

Rooms arbitrarily linked together using portals. Navigate the maze of rooms by
going through portals. If users are in the same room they can communicate with
each other using an ephemeral instant chat system.

## Technical high-level

This project is built in such a way to exemplify webdev in Haskell, while still
being a practical project used in a production setting. Here's a high level of
the parts of the application:

  * Daemon: built with Haskell. The daemon serves both the websocket server for
    the chat system, as well as a REST API for updating the static website.
    Handles authentication (JWT), management of the database, management of the
    static website.
  * Static website: The maze itself, which is produced and managed by the
    daemon. The static website has a lot of JavaScript which among other things
    is responsible for communicating with the REST API in order to access
    database information, authorization, and more things that can't simply be
    written once into a static website.

`CONTRIBUTING.md` has other information on this project.

No matter how you build or run this project, please try to use `nix-shell` and
`nix-build`. Please read the *Using `nix`* section below. In part, I aimed to
show off the helpfulness and usage of Nix.

### The `static/`and `built/` directories

The static website is built to `built/` (those files need to be served via an
HTTP service like Apache, nginx, or a dev server like `python3 -m http.serve`).
The Interwebz daemon itself does not handle serving the static website.

The files in `static` are static files used in building the site.

  * `static/mustache-build` are literal pages to build (like copying + running
    through parser)
  * `static/mustache` is simply for templates that get used to build pages.
  * `static/copy` are files that only get copied to `built/` and nothing else

## Testing

Try running `doctest` on `src`. It's also worth using `nix-shell`.

## Building and running an Interwebz server

You can run your own Interwebz server which has the potential to link to other Interwebz servers.

You have lots of options to build an Interwebz daemon binary, as well as options
for running an Interwebz server. Perhaps the most accessible and hands-off, but
production approach is to use Docker for everything. All of the options should
be mostly well documented here.

Your options for building the binary:

  * Let Docker handle it (good for production)
  * Build using only `cabal`
  * `nix-build`
  * `cabal` inside `nix-shell`

Here are some various options for running the daemon:

  * Let Docker handle it
  * Use `cabal run`
  * Run the built binary yourself (`./Interwebz` for example)

Here are some various options for serving the static files:

  * Let Docker handle it
  * Use `Test` mode which will enable the daemon to serve the static files
  * Use something like `simplehttp2server` or `python3 -m http.server` (dev/testing)
  * Your favorite HTTP daemon configured yourself (nginx, apache)
  * Some sort of newfangled "cloud" service

Here are some various options for the Postgres (database) daemon:

  * Let Docker handle it (Docker Compose; good for production)
  * Install it and set it up normally (good for production)
  * Set it up inside `nix-shell` (best for local dev/testing)
  * Some newfangled "cloud" service

I use Debian (unstable).

### ALWAYS: Generate the REST API's JWT keys

You will need to generate the private key for JSON Web Tokens (regardless how
you run) with these two commands:

```
openssl ecparam -name secp256k1 -genkey -noout -out jwt-priv-sig-key.pem
openssl ec -in jwt-priv-sig-key.pem -pubout > jwt-pub-sig-key.pem
```

I may include `openssl` in `nix-shell` in the future.

### Method 1: building, running using `nix`

Nix gives you one command to build the project and a shell/environment, all with
the same packages/tools/etc. at your disposal. That means you don't have to have
anything installed or configured except for having the Nix package manager
installed. This way we can be sure that we're all using the same versions and
that it (hopefully) works regardless of our operating system (as far as all the
tools and the project itself allows).

I'm using `nix-build` and `nix-shell` v2.10.3.

Build the project's daemon binary with:

```
nix-build release.nix
```

It should output the binary to `./result/bin/Interwebz`.

#### `nix-shell`

Enter the developer environment with the command:

```
nix-shell
```

You'll then be inside of an environment which gives you the same tools (Docker,
GHC, formatters, cabal, etc.) that I use.

It may seem like I use Stack, but the `stack.yaml` file is only here as a hacky
fix, if I remember correctly, for Nixpkgs.

For more information on how this project uses `nix-shell`, please see
`CONTRIBUTING.md` and also the *Method 3: vanilla* section.

### Method 2: running it all with Docker

This section shows you how to get Interwebz working entirely in/with Docker.
There is a Docker production config and a Docker testing config.

Be sure to start by editing an env file like `.env.dev`:

```
POSTGRES_USER=testpguser
POSTGRES_PASSWORD=testpguser

SCOTTY_ENV=Test
SCOTTY_SITE_TITLE=MazeQuest

SCOTTY_DATABASE_URL=postgres://testpguser:testpguser@db:5432/postgres

DEV_VOLUME_STATIC=./static:/opt/example/static:ro
```

This command will build all the dependencies as an image, so dependencies don't
have to be built/installed/downloaded every time:

```
docker build -f docker/Dockerfile-depends -t interwebz_depends .
```

This command will bring in new changes to the codebase and compile, then put up
the PostgreSQL service, as well as the API+static service (using nginx as
reverse proxy):

```
docker compose --verbose -f docker/docker-compose.yml -f docker/docker-compose.test.yml up
```

Now you can visit the website using either:

  * http://localhost:8080/new-room.html
  * type in the docker ip and can use port 80 (check IP with `docker inspect idofwebcontainer`)

You'll want to keep an eye on the PostgreSQL database volume, as well as the
built volume (which contains room images and the static site). Try `docker
volume ls`. Read more about volumes, including backing up and restoring, on [the
official Docker volumes
documentation](https://docs.docker.com/storage/volumes/#back-up-a-volume).

### Method 3: vanilla

This section is devoted to demonstrating how you can set up the server yourself.

Install the depends (you can skip this if you use `nix-shell`):

```
sudo apt install libjwt zlib1g-dev libpq-dev libjwt-dev
```

#### Vanilla step 1: Postgres

There are three different options for running a Postgres daemon (for the database).

I apologize for the messiness of this section. I will consolidate (there's not
much of a difference between the `nix-shell` commands and other method as I am
making it out to be at the moment) and clean up this information in the future.

##### Postgres in `nix-shell` (for dev/testing) or vanilla (without `nix-shell`)

You can run a developer/local test Postgres database like this with `nix-shell`:

If you plan to use a local development Postgres server, set up postgres in `nix-shell` like this:

```
[nix-shell:~/Projects/Interwebz]$ initdb -D .tmp/mydb
...
[nix-shell:~/Projects/Interwebz]$ pg_ctl -D .tmp/mydb -o "-k /tmp" -l logfile start
waiting for server to start.... done
server started
```

You can stop the above Postgres setup with `pg_ctl -D .tmp/mydb stop`.

On the other hand, if instead of the above you plan to setup a production
database on your machine you need to install postgres (I do this in Debian):

```
sudo apt install postgresql postgresql-contrib
```

Regardless if you are using `nix-shell` or not, you need to end your Postgres
setup like this, in order to configure the database:

```
psql -p 5432 -h localhost -e postgres -f docker/postgres/init.sql
```

##### Postgres using Docker (production)

If you just wanted to use the Docker PostgreSQL setup instead of the above for
Postgres you could do the below:

```
docker compose --verbose -f docker/docker-compose.yml -f docker/docker-compose.test.yml start db
```

#### Vanilla step 2: run the daemon

Run with Cabal:

```shell
env SCOTTY_ENV=Test SCOTTY_SITE_TITLE=IntraMaze PORT=8888 SCOTTY_DATABASE_URL=postgres://testpguser:testpguser@localhost:5432/testpgdatabase cabal run
```

You can also run using the binary built by `nix-build`:

```shell
env SCOTTY_ENV=Test SCOTTY_SITE_TITLE=IntraMaze PORT=8888 SCOTTY_DATABASE_URL=postgres://testpguser:testpguser@localhost:5432/testpgdatabase ./result/bin/Interwebz
```

The above will build the static files and run the REST API, which both manages
the database and handles updating the static files. The above command also tells
the daemon to run in `Test` mode, which will handle serving the static files for
us--the server will run on `8888`, you can now visit
[http://localhost:8888/login.html](http://localhost:8888/login.html).

Note that if you are using the Docker postgres setup you'll want to use
`SCOTTY_DATABASE_URL=postgres://testpguser:testpguser@localhost:5432/postgres`
instead. I don't know why (some Docker Compose Postgres image thing, maybe).

#### Vanilla step 3: serve the static files

You can skip this step if you're running the daemon in `Test` mode, as described
in *Vanilla step 2: run the daemon*.

The last step is to serve `built/` static files directory. For production it's
recommended you use something like nginx, Apache, or some kind of cloud service.
This section will only cover local development.

You can also use something like `simplehttp2server` or `python3 -m http.server`
to serve the `built/` directory (for testing and debugging).