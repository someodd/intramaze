# Interwebz

Random chat encounters in a collaboratively-built maze.

A maze made out of rooms users have made and arbitrarily linked together.
Explore the maze, if you bump into the other users in the same room you can
have an ephemeral conversation.

This all comes in the form of a static website which is managed through a REST
API and chat websocket daemon written in Haskell.

I use Debian (unstable).

## A bonus purpose

This is also made to be a demo of practical Haskell and Nix, demoing common web
technologies (JWT, REST, Mustache). I felt there wasn't enough Haskell and Nix
examples, especially not well documented ones, that show by example how to
complete something simple and practical like this project.

## Building notes

This project does not use Stack. The file is only here as a hacky fix.

## Project structure

### `static/`

The `static/mustache-build` are literal pages to build (like copying + running through parser), whereas `static/mustache` is simply for templates that get used to build pages.

## Testing

...

## Running

You will need to generate the private key for JSON Web Tokens (regardless how you run):

```
openssl ecparam -name secp256k1 -genkey -noout -out jwt-priv-sig-key.pem
openssl ec -in jwt-priv-sig-key.pem -pubout > jwt-pub-sig-key.pem
```

### Running with `nix`

Nix implementation is somewhat incomplete, especially `nix-shell`.

You can build with:

```
nix-build release.nix
```

You can enter the developer environment with:

```
nix-shell
```

You can even `cabal build` inside of the `nix-shell`! The shell comes loaded
with `haskell-langauge-server`, which you can use in combination with the
*Haskell* and *Nix Environment Selector* VSCode/VSCodium extensions.

The shell has postgres installed, but it's not set up yet (it will be!). I will
also set up Docker.

### Running with Docker

Be sure to start by editing an env file like `.env.dev`:

```
POSTGRES_USER=testpguser
POSTGRES_PASSWORD=testpguser

SCOTTY_ENV=Test
SCOTTY_SITE_TITLE=MazeQuest

SCOTTY_DATABASE_URL=postgres://testpguser:testpguser@db:5432/postgres

DEV_VOLUME_STATIC=./static:/opt/example/static:ro
```

This command will build all the dependencies as an image, so dependencies don't have to be built/installed/downloaded every time:

```
docker build -f docker/Dockerfile-depends -t interwebz_depends .
```

This command will bring in new changes to the codebase and compile, then put up the PostgreSQL service, as well as the
API+static service (using nginx as reverse proxy):

```
docker compose --verbose -f docker/docker-compose.yml -f docker/docker-compose.test.yml up
```

Now you can visit the website using either:

  * http://localhost:8080/new-room.html
  * type in the docker ip and can use port 80 (check IP with `docker inspect idofwebcontainer`)

You'll want to keep an eye on the PostgreSQL database volume, as well as the built volume (which contains room images and the static site). Try `docker volume ls`. Read more about volumes, including backing up and restoring, on [the official Docker volumes documentation](https://docs.docker.com/storage/volumes/#back-up-a-volume).

### Running on host

Install the depends:

```
sudo apt install libjwt zlib1g-dev postgresql postgresql-contrib libpq-dev libjwt-dev
```

Setup PostgreSQL:

```
sudo -u postgres psql
CREATE USER testpguser with PASSWORD 'testpguser';
CREATE DATABASE testpgdatabase WITH OWNER=testpguser;
```

... Of course if you just wanted to use the Docker PostgreSQL setup instead of
the above block you could do the below:

```
docker compose --verbose -f docker/docker-compose.yml -f docker/docker-compose.test.yml start db
```

Run the backend:

```shell
env SCOTTY_ENV=Test SCOTTY_SITE_TITLE=IntraMaze SCOTTY_DATABASE_URL=postgres://testpguser:testpguser@localhost:5432/postgres cabal run
```

The above will build the static files and run the REST API, which both manages
the database and handles updating the static files.

Serve the built static files directory (here's a way to test):

```
cd built
python3 -m http.server
```

Now you can visit http://localhost:8000/.

## Known bugs

The addresses expected for the REST API vs the static directory communicating
are shared/assume port 80 or 8080 and localhost, I think? So I need to be sure
to make it so addresses can be configured differently. This could be resolved
with CLI option to host `static` through the same app as REST API: https://hackage.haskell.org/package/wai-middleware-static-0.9.2/docs/Network-Wai-Middleware-Static.html

Is this actually a real issue? Test!
