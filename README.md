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

### The `static/` directory

The static website is actually built to `built/`. The files in `static` are
static files used in building the site.

  * `static/mustache-build` are literal pages to build (like copying + running
    through parser)
  * `static/mustache` is simply for templates that get used to build pages.
  * `static/copy` are files that only get copied to `built/` and nothing else

## Testing

Try running `doctest` on `src`.

## Building/setup/installation/running

This project gives you lots of options to work with when it comes to building
and messing around with the project. The easiest is probably Nix.

I use Debian (unstable).

### ALWAYS: Generate the REST API's JWT keys

You will need to generate the private key for JSON Web Tokens (regardless how
you run):

```
openssl ecparam -name secp256k1 -genkey -noout -out jwt-priv-sig-key.pem
openssl ec -in jwt-priv-sig-key.pem -pubout > jwt-pub-sig-key.pem
```

### Using `nix`

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
`CONTRIBUTING.md`.

### Running it all with Docker

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

### Running on host (vanilla!)

This section is devoted to demonstrating how you can set up the server yourself.

Install the depends (you can skip this if you use `nix-shell`):

```
sudo apt install libjwt zlib1g-dev postgresql postgresql-contrib libpq-dev libjwt-dev
```

#### 1. Postgres

There are three different options for running a Postgres daemon (for the database).

I apologize for the messiness of this section. I will consolidate and clean up
this information in the future. 

##### Postgres in `nix-shell`

You can run a developer/local test Postgres database like this with `nix-shell`:

```
[nix-shell:~/Projects/Interwebz]$ initdb -D .tmp/mydb
...
[nix-shell:~/Projects/Interwebz]$ pg_ctl -D .tmp/mydb -o "-k /tmp" -l logfile start
waiting for server to start.... done
server started
[nix-shell:~/Projects/Interwebz]$ psql -p 5432 -h localhost -e postgres
psql (14.4)
Type "help" for help.

postgres=# CREATE USER testpguser with PASSWORD 'testpguser';
CREATE USER testpguser with PASSWORD 'testpguser';
CREATE ROLE
postgres=# CREATE DATABASE testpgdatabase WITH OWNER=testpguser;
CREATE DATABASE testpgdatabase WITH OWNER=testpguser;
CREATE DATABASE
```

You can stop Postgres with `pg_ctl -D .tmp/mydb stop`.

##### Postgres without `nix-shell`

Even without `nix-shell` you can still use the same commands from the *Postgres in `nix-shell`* section. However, this demonstrates a more typical setup.

Setup PostgreSQL normally (no `nix-shell`):

```
sudo -u postgres psql
CREATE USER testpguser with PASSWORD 'testpguser';
CREATE DATABASE testpgdatabase WITH OWNER=testpguser;
```

##### Postgres using Docker

If you just wanted to use the Docker PostgreSQL setup instead of the above for
Postgres you could do the below:

```
docker compose --verbose -f docker/docker-compose.yml -f docker/docker-compose.test.yml start db
```

#### 2. Run the backend

Run with Cabal:

```shell
env SCOTTY_ENV=Test SCOTTY_SITE_TITLE=IntraMaze SCOTTY_DATABASE_URL=postgres://testpguser:testpguser@localhost:5432/postgres cabal run
```

You can also run using the binary built by `nix-build`:

```shell
env SCOTTY_ENV=Test SCOTTY_SITE_TITLE=IntraMaze SCOTTY_DATABASE_URL=postgres://testpguser:testpguser@localhost:5432/testpgdatabase ./result/bin/Interwebz
```

The above will build the static files and run the REST API, which both manages
the database and handles updating the static files.

Finally, serve the built static files directory (here's a way to test):

```
cd built
python3 -m http.server
```

Now you can visit http://localhost:8000/.

#### Known bugs running without Docker

The addresses expected for the REST API vs the static directory communicating
are shared/assume port 80 or 8080 and localhost, I think? So I need to be sure
to make it so addresses can be configured differently. This could be resolved
with CLI option to host `static` through the same app as REST API: https://hackage.haskell.org/package/wai-middleware-static-0.9.2/docs/Network-Wai-Middleware-Static.html

You may want to also use a reverse proxy like `nginx`. The software is also sort
of set up to expect HTTPS with the cookies.